#' R/auth.R


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
#' get_appropedia_username()
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
#' get_appropedia_password()
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
#' get_appropedia_api_url()
get_appropedia_api_url <- function() {
  Sys.getenv("APPROPEDIA_API_URL")
}


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
#' check_credentials()
check_credentials <- function() {
  username <- Sys.getenv("APPROPEDIA_API_USERNAME")
  password <- Sys.getenv("APPROPEDIA_API_PASSWORD")
  if (username == "")
    stop("APPROPEDIA_API_USERNAME not found")
  if (password == "")
    stop("APPROPEDIA_API_PASSWORD not found")
  invisible(TRUE)
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
  res <- GET(
    api_url,
    query = list(
      action = "query",
      meta = "tokens",
      type = "login",
      format = "json"
    ),
    handle = handle
  )
  token <- content(res)$query$tokens$logintoken
  if (is.null(token)) {
    stop("Failed to retrieve login token")
  }
  token
}


#' Log in to Appropedia
#'
#' Authenticates with the Appropedia MediaWiki API and returns an authenticated
#' session object for subsequent API requests.
#'
#' The login process performs the following steps:
#' \enumerate{
#'   \item Retrieve a login token.
#'   \item Submit username and password credentials.
#'   \item Retrieve a CSRF token for edit operations.
#'   \item Return an authenticated session object.
#' }
#'
#' User credentials are read from the environment variables
#' \code{APPROPEDIA_API_USERNAME} and
#' \code{APPROPEDIA_API_PASSWORD}.
#'
#' @param api_url Character string containing the MediaWiki API endpoint.
#' Defaults to the URL configured in the local environment.
#'
#' @return An object of class \code{wiki_session} containing:
#' \describe{
#'   \item{login_result}{Response returned by the MediaWiki login request.}
#'   \item{csrf_token}{CSRF token used for authenticated edit operations.}
#'   \item{handle}{An \code{httr} handle used to maintain the authenticated session.}
#' }
#'
#' @seealso \code{\link{check_credentials}},
#'   \code{\link{get_login_token}}
#'
#' @examples
#' \dontrun{
#' session <- do_login()
#' }
do_login <- function(api_url = get_appropedia_api_url(),
                     handle = httr::handle(get_appropedia_api_url())) {
  login_token <- get_login_token(api_url, handle)
  res <- POST(
    get_appropedia_api_url(),
    body = list(
      action = "login",
      lgname = get_appropedia_username(),
      lgpassword = get_appropedia_password(),
      lgtoken = login_token,
      format = "json"
    ),
    encode = "form",
    handle = handle
  )
  login_result <<- content(res)$login
  if (is.null(login_result) || login_result$result != "Success") {
    stop("Login failed")
  }
  res <- GET(
    get_appropedia_api_url(),
    query = list(
      action = "query",
      meta = "tokens",
      format = "json"
    ),
    handle = handle
  )
  csrf_token <- content(res)$query$tokens$csrftoken
  if (is.null(csrf_token)) {
    stop("Failed to retrieve CSRF token")
  }
  session <- list(
    login_result = login_result,
    csrf_token = csrf_token,
    handle =
      handle
  )
  class(session) <- "wiki_session"
  session
}


#' Check bot permissions for an authenticated session
#'
#' Verifies that the authenticated session has bot privileges.
#'
#' This function is primarily intended as a pre-flight check before performing
#' large automated edit operations. It sends a MediaWiki API request using
#' \code{assert=bot}, which causes the API to return an error if the session
#' is not authenticated as a bot account.
#'
#' @param session A \code{wiki_session} object returned by
#'   \code{\link{do_login}}.
#'
#' @return Logical. Returns \code{TRUE} if the session has bot permissions.
#' An error is raised if the assertion fails.
#'
#' @seealso \code{\link{do_login}}
#'
#' @examples
#' \dontrun{
#' session <- do_login()
#' check_bot_rights(session)
#' }
check_bot_rights <- function(session) {
  
  appropedia_query(
    list(
      action = "query",
      meta = "userinfo",
      assert = "bot",
      format = "json"
    ),
    handle = session$handle
  )
  
  invisible(TRUE)
}



#' Log out of the API
do_logout <- function(handle = httr::handle(get_appropedia_api_url())) {
  res <- POST(
    get_appropedia_api_url(),
    body = list(
      action = "logout",
      format = "json"
    ),
    encode = "form",
    handle = handle
  )

  invisible(TRUE)
}


