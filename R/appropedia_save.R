
#' Save a page through the MediaWiki API
#'
#' Performs an authenticated page edit using the MediaWiki API.
#'
#' This helper function centralizes API communication for operations that
#' require a CSRF token, such as page edits.
#'
#' @param page_name Character string containing the page title.
#' @param content Character string containing the new page content.
#' @param session A \code{wiki_session} object returned by
#'   \code{\link{do_login}}.
#' @param summary Edit summary recorded in the page history.
#' @param bot Logical indicating whether the edit should be marked as a bot
#' edit.
#'
#' @return The parsed API response.
#'
#' @seealso \code{\link{do_login}},
#'   \code{\link{appropedia_query}}
#'
#' @examples
#' \dontrun{
#' appropedia_save(
#'   page_name = "Sandbox",
#'   content = "Example edit",
#'   session = session
#' )
#' }
#' @export
appropedia_save <- function(
    page_name,
    content,
    session,
    summary = "Automatic maintenance edit",
    bot = TRUE
) {

  stop_if_invalid_session(session)

  res <- retry_request(httr::POST(
    get_appropedia_api_url(),
    body = list(
      action = "edit",
      title = page_name,
      text = content,
      summary = summary,
      token = session$csrf_token,
      format = "json",
      bot = as.integer(bot)
    ),
    encode = "form",
    handle = session$handle
  ))

  response <- httr::content(
    res,
    as = "parsed",
    type = "application/json"
  )

  if (!is.null(response$edit) &&
      response$edit$result == "Success") {
    return(TRUE)
  }

  print(response)

  FALSE
}

