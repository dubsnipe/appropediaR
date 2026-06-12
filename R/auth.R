#' R/auth.R

#' Get username from local environment.
get_appropedia_username <- function() {
  Sys.getenv("APPROPEDIA_API_USERNAME")
}


#' Get password from local environment.
get_appropedia_password <- function() {
  Sys.getenv("APPROPEDIA_API_PASSWORD")
}


#' Get Appropedia's API URL from local environment.
get_appropedia_api_url <- function() {
  Sys.getenv("APPROPEDIA_API_URL")
}


#' See if there are credentials stored
check_credentials <- function() {
  username <- Sys.getenv("APPROPEDIA_API_USERNAME")
  password <- Sys.getenv("APPROPEDIA_API_PASSWORD")
  if (username == "")
    stop("APPROPEDIA_API_USERNAME not found")
  if (password == "")
    stop("APPROPEDIA_API_PASSWORD not found")
  invisible(TRUE)
}


#' Obtain a login token.
#'
#' Helper function: it's called by do_login() when logging in.
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


#' Executing login. Do this to obtain API access.
#'
#' 1: Get login token
#' 2: Perform login
#' 3: Get CSRF token for edits
#' 4: Return a session object
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



#' Test the identity of a bot
#'
#' Debug
assert_bot <- function(handle = httr::handle(get_appropedia_api_url())) {
  GET(
    get_appropedia_api_url(),
    query = list(
      action = "query",
      meta = "userinfo",
      assert = "bot",
      format = "json"
    ),
    handle = handle
  )
  invisible(TRUE)
}


#' Logging out of the API
#'
#'
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


