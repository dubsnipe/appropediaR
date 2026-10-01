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
#' @param handle The API handle.
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
#'
#' @export
do_login <- function(api_url = get_appropedia_api_url(),
                     handle = httr::handle(get_appropedia_api_url())) {
  login_token <- get_login_token(api_url, handle)
  res <- httr::POST(
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
  login_result <- httr::content(res)$login
  if (is.null(login_result) || login_result$result != "Success") {
    stop("Login failed")
  }
  res <- httr::GET(
    get_appropedia_api_url(),
    query = list(
      action = "query",
      meta = "tokens",
      format = "json"
    ),
    handle = handle
  )
  csrf_token <- httr::content(res)$query$tokens$csrftoken
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
