#' Add multiple categories to uploaded files
#'
#' Adds one or more categories to multiple file description
#' pages using the existing batch category editor.
#'
#' @param files Character vector of file titles, or a data
#'   frame containing a title column.
#' @param add Character vector of categories to add.
#' @param session Authenticated MediaWiki session.
#' @param dry_run Preview changes without saving.
#' @param ... Additional arguments passed to
#'   batch_update_page_categories().
#'
#' @return Result from batch_update_page_categories().
#' @export
batch_update_file_categories <- function(
    files,
    add,
    session,
    dry_run = TRUE,
    ...
) {

  if (is.data.frame(files)) {
    if (!"title" %in% names(files)) {
      stop("files must contain a title column.")
    }
    files <- files$title
  }

  if (!is.character(files) || anyNA(files)) {
    stop("files must be a character vector of file titles.")
  }

  files <- unique(trimws(files))
  files <- files[nzchar(files)]

  if (length(files) == 0L) {
    stop("No file titles supplied.")
  }

  if (any(!startsWith(files, "File:"))) {
    stop("All titles must start with 'File:'.")
  }

  if (!is.character(add) || anyNA(add)) {
    stop("add must be a character vector of categories.")
  }

  add <- unique(trimws(add))
  add <- add[nzchar(add)]
  add <- sub("^Category:", "", add)

  if (length(add) == 0L) {
    stop("No categories supplied.")
  }

  changes <- data.frame(
    action = rep("add", length(add)),
    old_category = rep(NA_character_, length(add)),
    new_category = add,
    stringsAsFactors = FALSE
  )

  batch_update_page_categories(
    pages_list = files,
    changes = changes,
    session = session,
    dry_run = dry_run,
    ...
  )
}
