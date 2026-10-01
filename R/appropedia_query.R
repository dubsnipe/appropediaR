#' Execute a MediaWiki API query
#'
#' Performs a MediaWiki API request and returns the parsed JSON response.
#'
#' This helper function centralizes API communication for read operations that
#' do not require a CSRF token.
#'
#' @param query Named list containing MediaWiki API query parameters.
#' @param handle Optional \code{httr} handle used to maintain session state
#' across requests.
#'
#' @return A parsed JSON object represented as a nested R list.
#'
#' @seealso \code{\link{appropedia_save}}
#'
#' @examples
#'
#' \dontrun{
#' appropedia_query(
#'   list(
#'     action = "query",
#'     meta = "siteinfo",
#'     format = "json"
#'   )
#' )
#' }
#'
#' @export
# appropedia_query <- function(
#     query,
#     handle = NULL
# ) {
#
#   response <- httr::GET(
#     get_appropedia_api_url(),
#     query = query,
#     handle = handle
#   )
#
#   httr::stop_for_status(response)
#
#   jsonlite::fromJSON(
#     httr::content(response, as = "text", encoding = "UTF-8")
#   )
#
# }
appropedia_query <- function(
    query,
    handle = NULL
) {

  response <- httr::GET(
    get_appropedia_api_url(),
    query = query,
    handle = handle
  )

  httr::stop_for_status(response)

  content <- jsonlite::fromJSON(
    httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    )
  )

  if (!is.null(content$error)) {

    error_code <- if (!is.null(content$error$code)) {
      content$error$code
    } else {
      "unknown"
    }

    error_info <- if (!is.null(content$error$info)) {
      content$error$info
    } else {
      "No details returned."
    }

    stop(
      sprintf(
        "Appropedia API error `%s`: %s",
        error_code,
        error_info
      ),
      call. = FALSE
    )
  }

  content
}
