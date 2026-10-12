#' Get uploaded files by user or date range
#'
#' Retrieves current files from MediaWiki, optionally filtering by
#' the uploader of the latest version or a UTC date range.
#'
#' @param user Optional username.
#' @param start Optional starting date (YYYY-MM-DD).
#' @param end Optional ending date (YYYY-MM-DD), inclusive.
#' @param max_files Maximum files to retrieve. Inf for all.
#'
#' @return A tibble of current file metadata.
#' @export
get_files_by_upload <- function(
    user = NULL,
    start = NULL,
    end = NULL,
    max_files = Inf
) {

  if (!is.null(user) &&
      (length(user) != 1L || is.na(user) || !nzchar(user))) {
    stop("user must be a non-empty username.")
  }

  if (length(max_files) != 1L || is.na(max_files) ||
      max_files < 1 || (!is.infinite(max_files) &&
                        max_files != floor(max_files))) {
    stop("max_files must be a positive integer or Inf.")
  }

  parse_date <- function(x, label) {
    if (is.null(x)) return(NULL)

    if (length(x) != 1L || is.na(x) ||
        !grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(x))) {
      stop(label, " must use YYYY-MM-DD.")
    }

    date <- as.Date(x)
    if (is.na(date)) stop("Invalid ", label, ".")
    date
  }

  start <- parse_date(start, "start")
  end <- parse_date(end, "end")

  if (!is.null(start) && !is.null(end) && start > end) {
    stop("start must not be later than end.")
  }

  format_api_time <- function(x, end_of_day = FALSE) {
    if (is.null(x)) return(NULL)

    paste0(
      format(x, "%Y-%m-%d"),
      if (end_of_day) "T23:59:59Z" else "T00:00:00Z"
    )
  }

  query <- list(
    action = "query",
    list = "allimages",
    aisort = "timestamp",
    aidir = "older",
    ailimit = 500,
    aiprop = "timestamp|user|url|size|dimensions|mime|mediatype",
    format = "json",
    formatversion = 2
  )

  if (!is.null(user)) query$aiuser <- user

  # With aidir=older, API start must be the later timestamp.
  if (!is.null(end)) {
    query$aistart <- format_api_time(end, TRUE)
  }

  if (!is.null(start)) {
    query$aiend <- format_api_time(start)
  }

  results <- list()
  continuation <- NULL
  total <- 0L

  repeat {

    request <- query

    if (!is.null(continuation)) {
      request[names(continuation)] <- continuation
    }

    response <- appropedia_query(query = request)

    if (!is.null(response$error)) {
      stop("MediaWiki API error: ",
           response$error$info)
    }

    images <- response$query$allimages

    if (length(images) > 0L) {
      remaining <- max_files - total
      images <- utils::head(images, remaining)

      rows <- lapply(images, function(x) {
        tibble::tibble(
          title = paste0("File:", x$name),
          user = if (is.null(x$user)) NA_character_ else x$user,
          timestamp = if (is.null(x$timestamp)) NA_character_ else x$timestamp,
          url = if (is.null(x$url)) NA_character_ else x$url,
          size = if (is.null(x$size)) NA_real_ else x$size,
          width = if (is.null(x$width)) NA_integer_ else x$width,
          height = if (is.null(x$height)) NA_integer_ else x$height,
          mime = if (is.null(x$mime)) NA_character_ else x$mime,
          mediatype = if (is.null(x$mediatype)) NA_character_ else x$mediatype
        )
      })

      results <- c(results, rows)
      total <- total + length(images)
    }

    continuation <- response$continue

    if (is.null(continuation) || total >= max_files) {
      break
    }
  }

  if (length(results) == 0L) {
    return(tibble::tibble(
      title = character(),
      user = character(),
      timestamp = character(),
      url = character(),
      size = numeric(),
      width = integer(),
      height = integer(),
      mime = character(),
      mediatype = character()
    ))
  }

  dplyr::bind_rows(results)
}
