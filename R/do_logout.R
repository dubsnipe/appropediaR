#' Log out of the API
#' @param handle The API handle.
#' @examples
#' \dontrun{
#' session <- do_logout()
#' }
#'
#' @export
do_logout <- function(handle = httr::handle(get_appropedia_api_url())) {
  res <- httr::POST(
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
