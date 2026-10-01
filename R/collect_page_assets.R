#' Collect files associated with Appropedia pages
#'
#' Discovers files referenced directly in page wikitext and files introduced
#' by the rendered page. File metadata are resolved through the MediaWiki API,
#' but files are not downloaded.
#'
#' @param pages Character vector of Appropedia page titles.
#' @param embedded Include files embedded directly in page wikitext.
#' @param linked Include files linked directly through the `File:` or `Media:`
#'   namespaces.
#' @param transcluded Include files present in the rendered page but absent from
#'   direct source references. This includes files introduced by templates and
#'   other rendered content.
#' @param handle Optional `httr` handle reused across API requests.
#'
#' @return A data frame with one row per page-file relationship.
#' @export
collect_page_assets <- function(
    pages,
    embedded = TRUE,
    linked = TRUE,
    transcluded = TRUE,
    handle = NULL
) {
  if (!is.character(pages)) {
    stop("`pages` must be a character vector.", call. = FALSE)
  }

  flags <- list(embedded, linked, transcluded)
  valid_flags <- vapply(
    flags,
    function(value) is.logical(value) && length(value) == 1L && !is.na(value),
    logical(1)
  )

  if (!all(valid_flags)) {
    stop(
      "`embedded`, `linked`, and `transcluded` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  pages <- unique(trimws(pages))
  pages <- pages[nzchar(pages)]

  if (length(pages) == 0L || !any(unlist(flags, use.names = FALSE))) {
    return(.empty_page_assets())
  }

  relationships <- lapply(
    pages,
    .collect_page_asset_relationships,
    embedded = embedded,
    linked = linked,
    transcluded = transcluded,
    handle = handle
  )

  relationships <- do.call(rbind, relationships)

  if (is.null(relationships) || nrow(relationships) == 0L) {
    return(.empty_page_assets())
  }

  asset_rows <- !is.na(relationships$file_title)

  if (!any(asset_rows)) {
    return(relationships[, names(.empty_page_assets()), drop = FALSE])
  }

  metadata <- .get_file_metadata(
    unique(relationships$file_title[asset_rows]),
    handle = handle
  )

  metadata_index <- match(
    .file_key(relationships$file_title),
    .file_key(metadata$file_title)
  )

  discovery_status <- relationships$status
  discovery_error <- relationships$error

  metadata_columns <- setdiff(
    names(metadata),
    c("file_title", "status", "error")
  )

  for (column in metadata_columns) {
    relationships[[column]] <- metadata[[column]][metadata_index]
  }

  relationships$status <- discovery_status
  relationships$error <- discovery_error

  metadata_missing <- asset_rows & is.na(metadata_index)

  relationships$status[metadata_missing] <- ifelse(
    discovery_status[metadata_missing] == "partial",
    "partial",
    "unresolved"
  )
  relationships$error[metadata_missing] <- .combine_errors(
    discovery_error[metadata_missing],
    "File metadata were not returned by the Appropedia API."
  )

  metadata_problem <- asset_rows &
    !is.na(metadata_index) &
    metadata$status[metadata_index] != "ok"

  relationships$status[metadata_problem] <- ifelse(
    discovery_status[metadata_problem] == "partial",
    "partial",
    metadata$status[metadata_index[metadata_problem]]
  )

  relationships$error[metadata_problem] <- .combine_errors(
    discovery_error[metadata_problem],
    metadata$error[metadata_index[metadata_problem]]
  )

  relationships <- relationships[, names(.empty_page_assets()), drop = FALSE]
  rownames(relationships) <- NULL
  relationships
}
