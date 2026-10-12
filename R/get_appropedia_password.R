#' Get Appropedia password
#'
#' Retrieves the Appropedia password stored in the local environment.
#'
#' The password is read from the \code{APPROPEDIA_API_PASSWORD}
#' environment variable.
#'
#' @return A character string containing the configured password, or an empty
#' string if the environment variable is not set.
#'
#' @examples
#'\dontrun{
#' get_appropedia_password()
#'}
get_appropedia_password <- function() {
  Sys.getenv("APPROPEDIA_API_PASSWORD")
}
