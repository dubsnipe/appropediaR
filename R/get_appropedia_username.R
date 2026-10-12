#' Get Appropedia username
#'
#' Retrieves the Appropedia username stored in the local environment.
#'
#' The username is read from the \code{APPROPEDIA_API_USERNAME}
#' environment variable.
#'
#' @return A character string containing the configured username, or an empty
#' string if the environment variable is not set.
#'
#' @examples
#'\dontrun{
#' get_appropedia_username()
#'}
get_appropedia_username <- function() {
  Sys.getenv("APPROPEDIA_API_USERNAME")
}
