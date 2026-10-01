#' Resolve the final target of a redirect chain
#'
#' Starting from a page title, follows redirects until a non-redirect page
#' is reached.
#'
#' This function is useful when working with page lists that may contain
#' outdated titles or redirects.
#'
#' Redirect loops are detected automatically and processing stops if a cycle
#' is encountered.
#'
#' @param page_name Character string containing the page title.
#' @param seen Internal parameter used for redirect loop detection.
#'
#' @return Character string containing the final page title.
#'
#' @seealso
#' \code{\link{get_redirect_url}},
#' \code{\link{apply_redirects}}
#'
#' @examples
#' \dontrun{
#' resolve_redirect("Greywater")
#' }
#'
#' @export
resolve_redirect <- function(page_name, seen = character()) {

  # Prevent infinite loops if there's a redirect cycle
  if (page_name %in% seen) {
    return(page_name)
  }

  redirect_url <- get_redirect_url(page_name)

  if (!is.null(redirect_url) && nchar(redirect_url) > 0) {
    # Follow the redirect recursively
    return(resolve_redirect(redirect_url, c(seen, page_name)))
  } else {
    # No redirect, return final destination
    return(page_name)
  }
}
