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
