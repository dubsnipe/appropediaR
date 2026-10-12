#' Get Appropedia API URL
#'
#' Retrieves the Appropedia MediaWiki API endpoint stored in the local
#' environment.
#'
#' The URL is read from the \code{APPROPEDIA_API_URL}
#' environment variable.
#'
#' @return A character string containing the API URL, or an empty string if the
#' environment variable is not set.
#'
#' @examples
#'\dontrun{
#' get_appropedia_api_url()
#'}
get_appropedia_api_url <- function() {
  Sys.getenv("APPROPEDIA_API_URL")
}
