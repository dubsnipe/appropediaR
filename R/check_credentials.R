#' Check whether Appropedia credentials are configured
#'
#' Verifies that the required Appropedia environment variables are available.
#'
#' This function is primarily used before attempting authentication with the
#' MediaWiki API.
#'
#' @return Logical. Returns \code{TRUE} if the required credentials are
#' available and \code{FALSE} otherwise.
#'
#' @examples
#'\dontrun{
#' check_credentials()
#'}
#'
#' @export
check_credentials <- function() {
  username <- Sys.getenv("APPROPEDIA_API_USERNAME")
  password <- Sys.getenv("APPROPEDIA_API_PASSWORD")
  if (username == "")
    stop("APPROPEDIA_API_USERNAME not found")
  if (password == "")
    stop("APPROPEDIA_API_PASSWORD not found")
  invisible(TRUE)
}
