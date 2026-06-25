#' categories.R


#' Retrieve category memberships for one or more pages
#'
#' Queries the MediaWiki API and returns the categories assigned to one or more
#' pages.
#'
#' Multiple page titles may be supplied in a single request, allowing category
#' information to be collected efficiently in batches.
#'
#' @param page_names Character vector containing one or more page titles.
#'
#' @return A data frame with one row per page-category relationship and the
#' following columns:
#' \itemize{
#'   \item \code{page}: page title.
#'   \item \code{category}: category name.
#' }
#'
#' Category names are returned without the \code{"Category:"} prefix.
#'
#' @seealso
#' \code{\link{batch_get_page_categories}},
#' \code{\link{get_pages_from_category}}
#'
#' @examples
#' \dontrun{
#' get_categories_for_pages("Water")
#'
#' get_categories_for_pages(
#'   c("Water", "Ocean")
#' )
#' }
#'
#' @export
get_categories_for_pages <- function(page_names) {
  q <- list(action = "query",
            prop = "categories",
            titles = paste(page_names, collapse = "|"),
            format = "json"
  )

  json_res <- appropedia_query(query = q)
  pages <- json_res$query$pages

  result <- lapply(
    pages,
    function(page) {
      if (is.null(page$categories)){
        return(
          data.frame(
            page = page$title,
            category = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      }

      data.frame(
        page = page$title,
        category = sub("^Category:", "", page$categories$title),
        stringsAsFactors = FALSE
      )
    }
  )

  df <- do.call(rbind, result)
  rownames(df) <- NULL
  df

}


#' Retrieve categories for a large list of pages
#'
#' Retrieves category memberships for a large collection of pages by querying
#' the MediaWiki API in batches.
#'
#' This function automatically chunks requests, periodically saves progress to
#' a checkpoint file, and can resume interrupted workflows.
#'
#' @param pages_list Character vector containing page titles.
#' @param force_restart Logical indicating whether an existing checkpoint
#' should be deleted and processing restarted.
#' @param checkpoint_file Character string containing the checkpoint filename
#' or file path.
#' @param chunk_size Number of page titles to include in each API request.
#' @param checkpoint_interval Number of chunks processed between checkpoint
#' saves.
#' @param delete_temp Logical indicating whether the checkpoint file should be
#' removed after successful completion.
#'
#' @return A data frame with one row per page-category relationship.
#'
#' @seealso
#' \code{\link{get_categories_for_pages}},
#' \code{\link{checkpoint_manager}},
#' \code{\link{load_checkpoint}}
#'
#' @examples
#' \dontrun{
#' categories <- batch_get_page_categories(
#'   pages_list = get_all_pages()
#' )
#' }
#' @export
batch_get_page_categories <- function(pages_list,
                            force_restart = FALSE,
                            checkpoint_file = "categories_checkpoint.rds",
                            chunk_size = 50,
                            checkpoint_interval = 10,
                            delete_temp = TRUE) {


  chunked_pages_list <- split(pages_list, ceiling(seq_along(pages_list)/chunk_size))

  checkpoint <- load_checkpoint(
    checkpoint_file,
    force_restart,
    default_value = list(
      categories_batch = vector(
        "list",
        length(chunked_pages_list)
        ),
      next_index = 1
    ),
    input_length = length(pages_list)
  )

  categories_batch <- checkpoint$categories_batch
  start_i <- checkpoint$next_index

  for (i in start_i:length(chunked_pages_list)) {

    chunk <- chunked_pages_list[[i]]

    if (length(chunk) == 0 || all(is.na(chunk))) {
      next
    }

    categories_batch[[i]] <- get_categories_for_pages(chunk)

    next_index <- checkpoint_manager(
      i = i,
      input_size = length(chunked_pages_list),
      checkpoint_interval = checkpoint_interval,
      checkpoint_file = checkpoint_file,
      state = list(
        categories_batch = categories_batch,
        input_length = length(chunked_pages_list)
      )
    )
  }

  cleanup_checkpoint(checkpoint_file)

  # flatten result if needed
  do.call(rbind, categories_batch)
}


#' Count pages in a category
#'
#' Counts the number of pages belonging to a category using the MediaWiki API.
#'
#' This function can be useful for estimating workflow size before retrieving
#' page metadata or category memberships.
#'
#' @param category Character string containing the category name.
#' The \code{"Category:"} prefix is not required.
#' @param limit Maximum number of category members requested per API call.
#'
#' @return Integer containing the number of pages found in the category.
#'
#' @seealso
#' \code{\link{get_category_pages}}
#'
#' @examples
#' \dontrun{
#' count_pages_in_category("Water")
#' }
#'
#' @export
count_pages_in_category <- function(
    category,
    limit = 500
) {

  offset <- 0
  processed_total  <- 0

  repeat {

    query <- paste(
      c(
        paste0("[[Category:", category, "]]"),
        "limit=500",
        paste0("offset=", offset)
      ),
      collapse = "|"
    )

    cat("Querying offset", offset, "\n")

    res <- get_semantic_query(query)

    batch_size <- length(res$query$results)

    processed_total  <- processed_total + batch_size

    cat(
      "Retrieved", batch_size,
      "pages. Running total:", processed_total,
      "\n"
    )

    next_offset <- res[["query-continue-offset"]]

    if (is.null(next_offset)) {
      cat("Finished.\n")
      break
    }

    offset <- next_offset
  }

  return(processed_total)
}


#' Retrieve pages belonging to a category
#'
#' Retrieves the titles of pages belonging to a specified category using the
#' MediaWiki API.
#'
#' Results are automatically paginated until all category members have been
#' retrieved.
#'
#' @param category Character string containing the category name.
#' The \code{"Category:"} prefix is optional.
#' @param limit Maximum number of pages requested per API call.
#' @param namespace Namespace in numeric representation.
#' @param verbose Provide details during the run.
#'
#' @return Character vector containing page titles.
#'
#' @seealso
#' \code{\link{get_categories_for_pages}},
#' \code{\link{count_pages_in_category}}
#'
#' @examples
#' \dontrun{
#' pages <- get_pages_from_category("Water")
#' }
#'
#' @export
get_pages_from_category <- function(category,
                                    limit = 500,
                                    namespace = 0,
                                    verbose = TRUE) {

  all_pages <- character()
  cmcontinue <- NULL
  cmnamespace <- paste(namespace, collapse = "|")

  repeat {
    query <- list(
      action = "query",
      list = "categorymembers",
      cmtitle = paste0("Category:", category),
      cmlimit = limit,
      cmnamespace = cmnamespace,
      format = "json"
    )

    if (!is.null(cmcontinue)) {
      query$cmcontinue <- cmcontinue
    }

    data <- appropedia_query(query = query)
    pages <- data$query$categorymembers

    if (length(pages) > 0) {
      all_pages <- c(all_pages, pages$title)
    }

    if (verbose) { cat("Fetched total:", length(all_pages), "\n") }

    if (!is.null(data$continue$cmcontinue)) {
      cmcontinue <- data$continue$cmcontinue
    } else {
      break
    }
  }

  return(all_pages)
}



#' Extract explicit categories from page wikitext.
#'
#' Identifies category declarations that are explicitly present in page
#' content and returns their category names.
#'
#' Category names are extracted from MediaWiki category tags such as:
#'
#' \preformatted{
#' [[Category:Water]]
#' [[Category:Projects]]
#' }
#'
#' Category declarations are matched case-insensitively, so the following
#' are treated equivalently:
#'
#' \preformatted{
#' [[Category:Water]]
#' [[category:Water]]
#' [[CATEGORY:Water]]
#' }
#'
#' Categories added through template transclusion are not detected because
#' they do not exist directly in the page wikitext.
#'
#' @param content Character string containing page wikitext.
#'
#' @return A character vector containing explicit category names. Returns
#' an empty character vector if no explicit categories are found.
#'
#' Category sort keys are not currently preserved. For example:
#'
#' \preformatted{
#' [[Category:Projects|Solar distiller]]
#' }
#'
#' will return "Projects".
#'
#' @examples
#' \dontrun{
#' content <- "
#' {{Page data}}
#'
#' [[Category:Water]]
#' [[Category:Projects]]
#' "
#'
#' extract_explicit_categories(content)
#' }
#'
#' @export
extract_explicit_categories <- function(content) {

  matches <- stringr::str_match_all(
    content,
    "\\[\\[(?i:category):([^\\]|]+)"
  )[[1]]

  if (nrow(matches) == 0) {
    return(character())
  }

  unique(trimws(matches[, 2]))
}



#' Modify explicit category declarations in page content.
#'
#' Applies add, delete and rename operations to explicit category tags found
#' in the page wikitext. Categories added through templates are not modified.
#'
#' @param content Character string containing page wikitext.
#' @param changes Data frame with columns:
#'   action, old_category, new_category.
#'
#' @return A list containing:
#' \itemize{
#'   \item content
#'   \item explicit_before
#'   \item explicit_after
#'   \item added
#'   \item removed
#'   \item renamed
#'   \item changed
#' }
#'
#' @export
modify_categories <- function(
    content,
    changes
) {

  explicit_before <- extract_explicit_categories(
    content
  )

  explicit_after <- explicit_before

  added <- character()
  removed <- character()

  renamed <- data.frame(
    old_category = character(),
    new_category = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(changes))) {

    action <- changes$action[i]

    if (action == "delete") {

      category <- changes$old_category[i]

      if (category %in% explicit_after) {

        explicit_after <- setdiff(
          explicit_after,
          category
        )

        removed <- c(removed, category)
      }

    } else if (action == "rename") {

      old_category <- changes$old_category[i]
      new_category <- changes$new_category[i]

      if (old_category %in% explicit_after) {

        explicit_after[
          explicit_after == old_category
        ] <- new_category

        renamed <- rbind(
          renamed,
          data.frame(
            old_category = old_category,
            new_category = new_category,
            stringsAsFactors = FALSE
          )
        )
      }

    } else if (action == "add") {

      category <- changes$new_category[i]

      if (!(category %in% explicit_after)) {

        explicit_after <- c(
          explicit_after,
          category
        )

        added <- c(
          added,
          category
        )
      }
    }
  }

  modified_content <- content

  #
  # Remove all explicit category tags
  #
  modified_content <- gsub(
    "\\[\\[(?i:category):[^\\]]+\\]\\]\\s*",
    "",
    modified_content,
    perl = TRUE
  )

  #
  # Append rebuilt explicit category block
  #
  if (length(explicit_after) > 0) {

    category_text <- paste0(
      "\n[[Category:",
      explicit_after,
      "]]",
      collapse = ""
    )

    modified_content <- paste0(
      trimws(modified_content),
      "\n",
      category_text,
      "\n"
    )
  }

  list(
    content = modified_content,
    explicit_before = explicit_before,
    explicit_after = explicit_after,
    added = added,
    removed = removed,
    renamed = renamed,
    changed = !identical(
      content,
      modified_content
    )
  )
}


#' Update explicit categories on a page.
#'
#' Reads page wikitext, applies category modifications and optionally saves
#' the result through the MediaWiki API.
#'
#' Only explicit category declarations present in the page wikitext are
#' modified. Categories added through template transclusion are reported but
#' not edited.
#'
#' @param page_name Character string containing the page title.
#' @param changes Data frame containing category modifications.
#'   Expected columns:
#'   \itemize{
#'     \item action
#'     \item old_category
#'     \item new_category
#'   }
#' @param session MediaWiki session object.
#' @param dry_run Logical indicating whether edits should be saved.
#'
#' @return A list containing the modification report.
#'
#' @seealso
#' \code{\link{modify_categories}},
#' \code{\link{extract_explicit_categories}},
#' \code{\link{get_page_content}}
#'
#' @export
update_page_categories <- function(
    page_name,
    changes,
    session,
    dry_run = TRUE
) {

  content <- get_page_content(page_name)

  if (is.null(content)) {

    return(list(
      page_name = page_name,
      changed = FALSE,
      saved = FALSE,
      status = "page_not_found",
      timestamp = Sys.time()
    ))
  }

  category_report <- modify_categories(
    content = content,
    changes = changes
  )

  if (!category_report$changed) {

    return(list(
      page_name = page_name,
      changed = FALSE,
      saved = FALSE,
      status = "no_change",
      explicit_before = category_report$explicit_before,
      explicit_after = category_report$explicit_after,
      timestamp = Sys.time()
    ))
  }

  if (!dry_run) {

    appropedia_save(
      page_name = page_name,
      content = category_report$content,
      session = session,
      summary = "Updating categories"
    )

    saved <- TRUE

  } else {

    saved <- FALSE
  }

  c(
    list(
      page_name = page_name,
      saved = saved,
      status = ifelse(
        dry_run,
        "dry_run",
        "saved"
      ),
      timestamp = Sys.time()
    ),
    category_report
  )
}
