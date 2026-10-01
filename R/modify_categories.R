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
