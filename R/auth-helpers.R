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


#' Obtain a MediaWiki login token
#'
#' Retrieves a login token from the MediaWiki API.
#' This is an internal helper function used by \code{do_login()} as part of
#' the MediaWiki authentication process.
#'
#' @param api_url Character string containing the MediaWiki API endpoint.
#' Defaults to the URL configured in the local environment.
#'@param handle Optional \code{httr} handle object used to maintain session
#'state across requests.
#'
#'@return A character string containing the login token.
#'
#'@seealso \code{\link{do_login}}
#'
#'@examples
#'\dontrun{
#'get_login_token()
#'}
get_login_token <- function(api_url = get_appropedia_api_url(),
                            handle = httr::handle(get_appropedia_api_url())) {
  res <- httr::GET(
    api_url,
    query = list(
      action = "query",
      meta = "tokens",
      type = "login",
      format = "json"
    ),
    handle = handle
  )
  token <- httr::content(res)$query$tokens$logintoken
  if (is.null(token)) {
    stop("Failed to retrieve login token")
  }
  token
}
