#' Get the immediate redirect target for a page
#'
#' Retrieves the direct redirect target of a page using the MediaWiki API.
#'
#' This helper function is used by \code{\link{resolve_redirect}} to follow
#' redirect chains until a final destination page is reached.
#'
#' @param page_name Character string containing the page title.
#' @param api_url Character string containing the MediaWiki API endpoint.
#'
#' @return Character string containing the redirect target if the page is a
#' redirect, or \code{NULL} if the page is not a redirect.
#'
#' @seealso \code{\link{resolve_redirect}}
#'
#' @examples
#' \dontrun{
#' get_redirect_url("Greywater")
#' }
get_redirect_url <- function(page_name,
                             api_url = get_appropedia_api_url()) {
  tryCatch({
    res <- httr::GET(api_url, query = list(
      action = "query",
      prop = "info",
      redirects="",
      titles = page_name,
      format = "json"
    ), httr::timeout(5))

    pages <- httr::content(res, as = "parsed",
                     type = "application/json"
    )

    if (!is.null(pages$query$redirects[[1]]$to)) {
      response <-
        stringr::str_replace_all(pages$query$redirects[[1]]$to, " ", "_")
      return(response)
    } else {
      return(NULL)  # Explicitly return NULL for missing wikitext
    }
  })
}
