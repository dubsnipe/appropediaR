#' Get all current files from Appropedia
#'
#' Retrieves metadata for all current files registered in Appropedia using
#' the MediaWiki `allimages` API module. Results are fetched in batches using
#' API continuation until all files have been returned.
#'
#' Only the current version of each file is returned. Archived file revisions,
#' deleted files, and generated thumbnails are not included in the inventory.
#'
#' Returned metadata includes the filename, upload timestamp, uploader, direct
#' URL, file size, dimensions when applicable, SHA-1 hash, MIME type, and media
#' type.
#'
#' @param limit Integer. Maximum number of files requested per API call.
#'   Defaults to `500`.
#' @param handle Optional curl handle passed to [appropedia_query()].
#' @param verbose Logical. If `TRUE`, prints progress while batches are fetched.
#'   Defaults to `TRUE`.
#'
#' @return A data frame with one row per current Appropedia file.
#'
#' @seealso [appropedia_query()]
#'
#' @examples
#' \dontrun{
#' files <- get_all_files()
#' files <- get_all_files(verbose = FALSE)
#' }
#'
#' @export
get_all_files <- function(
    limit = 500,
    handle = NULL,
    verbose = TRUE
) {

  batches <- list()
  cont <- NULL
  batch_number <- 0
  total_files <- 0

  if (verbose) {
    message("Fetching files from Appropedia...")
  }

  repeat {

    batch_number <- batch_number + 1

    if (verbose) {
      message("Fetching batch ", batch_number, "...")
    }

    q <- list(
      action = "query",
      list = "allimages",
      ailimit = limit,
      aiprop = paste(
        c(
          "timestamp",
          "user",
          "url",
          "size",
          "sha1",
          "mime",
          "mediatype"
        ),
        collapse = "|"
      ),
      format = "json",
      formatversion = 2
    )

    if (!is.null(cont)) {
      q <- c(q, cont)
    }

    dat <- appropedia_query(
      query = q,
      handle = handle
    )

    if (!is.null(dat$error)) {
      stop("API error: ", dat$error$info)
    }

    batch <- dat$query$allimages

    batches[[batch_number]] <- batch

    batch_size <- nrow(batch)
    total_files <- total_files + batch_size

    if (verbose) {
      message(
        "  Retrieved ",
        batch_size,
        " files (",
        total_files,
        " total)"
      )
    }

    if (is.null(dat$continue)) {
      break
    }

    cont <- dat$continue
  }

  all_names <- Reduce(
    union,
    lapply(batches, names)
  )

  batches <- lapply(
    batches,
    function(x) {
      missing <- setdiff(all_names, names(x))
      x[missing] <- NA
      x[all_names]
    }
  )

  files <- do.call(rbind, batches)

  rownames(files) <- NULL

  if (verbose) {
    message(
      "Finished: ",
      nrow(files),
      " files retrieved in ",
      batch_number,
      " batches."
    )
  }

  files
}
