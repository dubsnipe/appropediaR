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
#'
#' @export
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
