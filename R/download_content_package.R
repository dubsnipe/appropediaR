#' Download an Appropedia content package
#'
#' Creates one self-contained directory per page, writes the current page
#' wikitext and rendered HTML, and downloads each discovered file into a
#' directory based on its asset class. The same file is downloaded separately
#' for every page that uses it.
#'
#' @param pages Character vector of Appropedia page titles.
#' @param path Directory in which to create the content package.
#' @param assets Optional data frame returned by [collect_page_assets()]. When
#'   `NULL`, assets are collected before downloading.
#' @param embedded,linked,transcluded Passed to [collect_page_assets()] when
#'   `assets` is `NULL`.
#' @param include_wikitext Write the current page wikitext.
#' @param include_html Write rendered page HTML.
#' @param overwrite Overwrite existing page and asset files.
#' @param handle Optional `httr` handle reused across Appropedia API requests.
#'
#' @return A data frame describing every page-content write and asset-download
#'   attempt. The same data are written to `download-report.csv` in `path`.
#' @export
download_content_package <- function(
    pages,
    path,
    assets = NULL,
    embedded = TRUE,
    linked = TRUE,
    transcluded = TRUE,
    include_wikitext = TRUE,
    include_html = TRUE,
    overwrite = FALSE,
    handle = NULL
) {
  if (!is.character(pages)) {
    stop("`pages` must be a character vector.", call. = FALSE)
  }

  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be one non-empty character string.", call. = FALSE)
  }

  flags <- list(
    embedded = embedded,
    linked = linked,
    transcluded = transcluded,
    include_wikitext = include_wikitext,
    include_html = include_html,
    overwrite = overwrite
  )

  valid_flags <- vapply(
    flags,
    function(value) is.logical(value) && length(value) == 1L && !is.na(value),
    logical(1)
  )

  if (!all(valid_flags)) {
    stop(
      paste0(
        "`embedded`, `linked`, `transcluded`, `include_wikitext`, ",
        "`include_html`, and `overwrite` must be TRUE or FALSE."
      ),
      call. = FALSE
    )
  }

  pages <- unique(trimws(pages))
  pages <- pages[nzchar(pages)]

  if (length(pages) == 0L) {
    stop("`pages` must contain at least one page title.", call. = FALSE)
  }

  if (is.null(assets)) {
    assets <- collect_page_assets(
      pages,
      embedded = embedded,
      linked = linked,
      transcluded = transcluded,
      handle = handle
    )
  } else {
    .validate_content_package_assets(assets)
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  package_path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  page_content <- lapply(
    pages,
    function(page) {
      tryCatch(
        .get_page_export_content(
          page,
          include_wikitext = include_wikitext,
          include_html = include_html,
          handle = handle
        ),
        error = function(error) {
          list(
            requested_title = page,
            page_title = page,
            page_id = NA_integer_,
            page_revision_id = NA_integer_,
            wikitext = NA_character_,
            html = NA_character_,
            status = "page_error",
            error = conditionMessage(error)
          )
        }
      )
    }
  )

  page_table <- .prepare_page_directories(page_content)

  for (page_directory in page_table$page_directory) {
    page_path <- file.path(package_path, page_directory)

    directories <- c(
      "wikitext",
      "html",
      "images",
      "manufacturing",
      "documents",
      "other"
    )

    invisible(lapply(
      file.path(page_path, directories),
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    ))
  }

  page_report <- .write_page_content(
    page_content,
    page_table,
    package_path = package_path,
    include_wikitext = include_wikitext,
    include_html = include_html,
    overwrite = overwrite
  )

  asset_report <- .download_content_assets(
    assets,
    page_table,
    package_path = package_path,
    overwrite = overwrite
  )

  report <- rbind(page_report, asset_report)
  rownames(report) <- NULL

  report_path <- file.path(package_path, "download-report.csv")
  utils::write.csv(
    report,
    report_path,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )

  attr(report, "package_path") <- package_path
  attr(report, "report_path") <- report_path
  report
}
