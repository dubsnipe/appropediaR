#' Transform reviewed categories in page wikitext
#'
#' Applies category deletion, renaming, splitting, and migration into template
#' parameters to a single page in memory. The function performs no API calls
#' and cannot save changes.
#'
#' @param content Character string containing page wikitext.
#' @param category_plan Data frame containing one row per reviewed category.
#'   It must contain `category`. Supported optional columns are `delete`,
#'   `rename`, `add or split`, and logical facet columns referenced by
#'   `parameter_map`.
#' @param parameter_map Data frame defining which facets migrate to template
#'   parameters. It must contain `facet`, `template_name`, and `param_name`.
#'   An optional `separator` column controls how multiple values are stored;
#'   the default is `; `.
#' @param api_categories Optional character vector containing all categories
#'   reported by the MediaWiki API. Categories present here but absent from the
#'   wikitext are reported as implicit and are not changed.
#' @param split_delimiter Regular expression used to split values in the
#'   `add or split` column. The default expects a vertical bar.
#' @param merge One of `append`, `replace`, or `skip`. Controls what happens
#'   when a target parameter already contains a value.
#'
#' @return A list containing transformed content, before/after category lists,
#'   proposed parameter changes, snippets, warnings, and status information.
#'
#' @details
#' Category actions are composed before the page is rewritten. `delete` takes
#' precedence. Otherwise, `add or split` takes precedence over `rename`.
#' Categories mapped to a template parameter are removed from the explicit
#' category block after their transformed values have been migrated.
#'
#' The function uses the package's current regex-based template modifier. Pages
#' containing nested templates or other risky markup are flagged for manual
#' review in `warnings`.
#'
#' @export
transform_page_categories <- function(
    content,
    category_plan,
    parameter_map,
    api_categories = NULL,
    split_delimiter = "\\s*\\|\\s*",
    merge = c("append", "replace", "skip")
) {
  merge <- match.arg(merge)

  if (!is.character(content) || length(content) != 1L || is.na(content)) {
    stop("content must be one non-missing character string")
  }
  if (!is.data.frame(category_plan) || !"category" %in% names(category_plan)) {
    stop("category_plan must be a data frame containing `category`")
  }
  required_map <- c("facet", "template_name", "param_name")
  if (!is.data.frame(parameter_map) ||
      !all(required_map %in% names(parameter_map))) {
    stop(
      "parameter_map must contain `facet`, `template_name`, and `param_name`"
    )
  }

  warnings <- .category_migration_warnings(content)
  explicit_before <- extract_explicit_categories(content)
  api_categories <- unique(stats::na.omit(api_categories))
  implicit_categories <- setdiff(api_categories, explicit_before)

  plan <- category_plan[category_plan$category %in% explicit_before, , drop = FALSE]
  category_changes <- data.frame(
    action = character(),
    old_category = character(),
    new_category = character(),
    stringsAsFactors = FALSE
  )
  migrations <- list()
  conflicts <- character()

  for (i in seq_len(nrow(plan))) {
    source <- plan$category[[i]]
    delete_value <- "delete" %in% names(plan) && isTRUE(plan$delete[[i]])

    if (delete_value) {
      category_changes <- rbind(
        category_changes,
        data.frame(
          action = "delete",
          old_category = source,
          new_category = NA_character_,
          stringsAsFactors = FALSE
        )
      )
      next
    }

    values <- .planned_category_values(
      row = plan[i, , drop = FALSE],
      split_delimiter = split_delimiter
    )

    mapped_facets <- parameter_map$facet[
      vapply(
        parameter_map$facet,
        function(facet) {
          facet %in% names(plan) && isTRUE(plan[[facet]][[i]])
        },
        logical(1)
      )
    ]

    if (length(mapped_facets) > 1L) {
      conflicts <- c(
        conflicts,
        paste0(
          source,
          " maps to multiple parameter facets: ",
          paste(mapped_facets, collapse = ", ")
        )
      )
      next
    }

    if (length(mapped_facets) == 1L) {
      map_row <- parameter_map[
        parameter_map$facet == mapped_facets,
        ,
        drop = FALSE
      ][1, , drop = FALSE]

      migrations[[length(migrations) + 1L]] <- data.frame(
        source_category = source,
        facet = mapped_facets,
        template_name = map_row$template_name,
        param_name = map_row$param_name,
        separator = if ("separator" %in% names(map_row)) {
          map_row$separator
        } else {
          "; "
        },
        value = values,
        stringsAsFactors = FALSE
      )

      next
    }

    if (!identical(values, source)) {
      category_changes <- rbind(
        category_changes,
        data.frame(
          action = "delete",
          old_category = source,
          new_category = NA_character_,
          stringsAsFactors = FALSE
        ),
        data.frame(
          action = "add",
          old_category = NA_character_,
          new_category = values,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  migration_df <- if (length(migrations)) {
    do.call(rbind, migrations)
  } else {
    data.frame(
      source_category = character(), facet = character(),
      template_name = character(), param_name = character(),
      separator = character(), value = character(),
      stringsAsFactors = FALSE
    )
  }

  if (length(conflicts)) {
    return(list(
      content = content,
      changed = FALSE,
      status = "conflict",
      explicit_before = explicit_before,
      explicit_after = explicit_before,
      implicit_categories = implicit_categories,
      category_changes = category_changes,
      parameter_changes = data.frame(
        template_name = character(), param_name = character(),
        before = character(), after = character(), action = character(),
        stringsAsFactors = FALSE
      ),
      conflicts = unique(conflicts),
      warnings = unique(warnings),
      template_snippet_before = .migration_template_snippet(content, parameter_map),
      template_snippet_after = .migration_template_snippet(content, parameter_map),
      category_block_before = .category_block_snippet(content),
      category_block_after = .category_block_snippet(content)
    ))
  }

  modified_content <- content
  parameter_changes <- list()

  if (nrow(migration_df) > 0L) {
    groups <- split(
      migration_df,
      paste(migration_df$template_name, migration_df$param_name, sep = "\r")
    )

    for (group in groups) {
      template_name <- group$template_name[[1]]
      param_name <- group$param_name[[1]]
      separator <- group$separator[[1]]
      proposed_values <- unique(trimws(group$value[nzchar(trimws(group$value))]))

      current_value <- .extract_template_parameter(
        modified_content,
        template_name,
        param_name
      )

      final_value <- .merge_parameter_values(
        current_value = current_value,
        proposed_values = proposed_values,
        separator = separator,
        merge = merge
      )

      if (identical(merge, "skip") && nzchar(current_value)) {
        parameter_changes[[length(parameter_changes) + 1L]] <- data.frame(
          template_name = template_name,
          param_name = param_name,
          before = current_value,
          after = current_value,
          action = "skipped_existing",
          stringsAsFactors = FALSE
        )
        next
      }

      result <- modify_template(
        content = modified_content,
        new_value = final_value,
        template_name = template_name,
        param_name = param_name
      )

      action <- if (!result$template_found) {
        warnings <- c(
          warnings,
          paste0("Template not found: ", template_name)
        )
        "template_not_found"
      } else if (!result$changed) {
        "unchanged"
      } else if (result$parameter_added) {
        "added"
      } else {
        "updated"
      }

      if (result$template_found && !identical(action, "skipped_existing")) {
        category_changes <- rbind(
          category_changes,
          data.frame(
            action = "delete",
            old_category = unique(group$source_category),
            new_category = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      }

      modified_content <- result$content
      parameter_changes[[length(parameter_changes) + 1L]] <- data.frame(
        template_name = template_name,
        param_name = param_name,
        before = current_value,
        after = if (result$template_found) final_value else current_value,
        action = action,
        stringsAsFactors = FALSE
      )
    }
  }

  parameter_changes <- if (length(parameter_changes)) {
    do.call(rbind, parameter_changes)
  } else {
    data.frame(
      template_name = character(), param_name = character(),
      before = character(), after = character(), action = character(),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(category_changes) > 0L) {
    category_result <- modify_categories(
      content = modified_content,
      changes = category_changes
    )
  } else {
    category_result <- list(
      content = modified_content,
      explicit_after = extract_explicit_categories(modified_content)
    )
  }

  final_content <- category_result$content
  changed <- !identical(content, final_content)
  status <- if (changed) "dry_run_changed" else "no_change"

  list(
    content = final_content,
    changed = changed,
    status = status,
    explicit_before = explicit_before,
    explicit_after = category_result$explicit_after,
    implicit_categories = implicit_categories,
    category_changes = category_changes,
    parameter_changes = parameter_changes,
    conflicts = unique(conflicts),
    warnings = unique(warnings),
    template_snippet_before = .migration_template_snippet(content, parameter_map),
    template_snippet_after = .migration_template_snippet(final_content, parameter_map),
    category_block_before = .category_block_snippet(content),
    category_block_after = .category_block_snippet(final_content)
  )
}

.planned_category_values <- function(row, split_delimiter) {
  source <- row$category[[1]]

  split_value <- if ("add or split" %in% names(row)) {
    row[["add or split"]][[1]]
  } else {
    NA
  }
  rename_value <- if ("rename" %in% names(row)) row$rename[[1]] else NA

  if (is.list(split_value)) {
    split_values <- unlist(split_value, use.names = FALSE)
  } else if (!is.na(split_value) && nzchar(trimws(split_value))) {
    split_values <- strsplit(split_value, split_delimiter, perl = TRUE)[[1]]
  } else {
    split_values <- character()
  }

  split_values <- unique(trimws(split_values[nzchar(trimws(split_values))]))
  if (length(split_values)) {
    return(split_values)
  }
  if (!is.na(rename_value) && nzchar(trimws(rename_value))) {
    return(trimws(rename_value))
  }
  source
}

.category_migration_warnings <- function(content) {
  patterns <- c(
    nowiki = "<nowiki\\b",
    pre = "<pre\\b",
    syntaxhighlight = "<syntaxhighlight\\b",
    html_comment = "<!--",
    nested_template = "\\{\\{[^{}]*\\{\\{"
  )
  names(patterns)[vapply(
    patterns,
    grepl,
    logical(1),
    x = content,
    ignore.case = TRUE,
    perl = TRUE
  )]
}

.extract_template_parameter <- function(content, template_name, param_name) {
  template_esc <- gsub(
    "([.|()\\^{}+$*?]|\\[|\\])",
    "\\\\\\1",
    template_name
  )
  param_esc <- gsub(
    "([.|()\\^{}+$*?]|\\[|\\])",
    "\\\\\\1",
    param_name
  )
  pattern <- paste0(
    "\\{\\{", template_esc, "[^}]*?\\|\\s*", param_esc,
    "\\s*=\\s*([^|}]*)"
  )
  match <- regexec(pattern, content, ignore.case = TRUE, perl = TRUE)
  value <- regmatches(content, match)[[1]]
  if (length(value) < 2L) "" else trimws(value[[2]])
}

.merge_parameter_values <- function(
    current_value,
    proposed_values,
    separator,
    merge
) {
  if (merge == "replace" || !nzchar(current_value)) {
    return(paste(proposed_values, collapse = separator))
  }
  if (merge == "skip") {
    return(current_value)
  }

  existing <- trimws(strsplit(current_value, separator, fixed = TRUE)[[1]])
  paste(unique(c(existing, proposed_values)), collapse = separator)
}

.category_block_snippet <- function(content) {
  matches <- regmatches(
    content,
    gregexpr(
      "\\[\\[(?i:category):[^]]+\\]\\]",
      content,
      perl = TRUE
    )
  )[[1]]
  if (identical(matches, character(0)) || identical(matches, "")) "" else {
    paste(matches, collapse = "\n")
  }
}

.migration_template_snippet <- function(content, parameter_map) {
  templates <- unique(parameter_map$template_name)
  snippets <- vapply(
    templates,
    function(template_name) {
      template_esc <- gsub(
        "([.|()\\^{}+$*?]|\\[|\\])",
        "\\\\\\1",
        template_name
      )
      pattern <- paste0("\\{\\{", template_esc, "[^}]*}}")
      match <- regexpr(pattern, content, ignore.case = TRUE, perl = TRUE)
      if (match[[1]] == -1L) "" else regmatches(content, match)
    },
    character(1)
  )
  snippets <- snippets[nzchar(snippets)]
  paste(snippets, collapse = "\n\n")
}
