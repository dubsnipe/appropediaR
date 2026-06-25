#' read.R


#' Retrieve the wikitext of a page
#'
#' Retrieves the current wikitext content of a page using the MediaWiki API.
#'
#' This function is commonly used before performing template updates,
#' content analysis, or bulk editing operations.
#'
#' @param page_name Character string containing the page title.
#' @param handle Optional \code{httr} handle used to maintain session state
#' across requests.
#'
#' @return Character string containing the page wikitext.
#'
#' Returns \code{NULL} if the page cannot be retrieved or if no page content
#' is available.
#'
#' @seealso
#' \code{\link{update_template_parameter}},
#' \code{\link{appropedia_query}}
#'
#' @examples
#' \dontrun{
#' text <- get_page_content("Water")
#'
#' cat(substr(text, 1, 500))
#' }
get_page_content <- function(
    page_name,
    handle = NULL
) {

  q <- list(
    action = "query",
    prop = "revisions",
    rvslots = "main",
    rvprop = "content",
    titles = page_name,
    format = "json"
  )

  res <- tryCatch(
    appropedia_query(
      q,
      handle = handle
    ),
    error = function(e) {
      return(NULL)
    }
  )

  if (is.null(res)) {
    return(NULL)
  }

  pages <- res$query$pages
  page_id <- names(pages)[1]
  revisions <- pages[[page_id]]$revisions

  if (is.null(revisions)) {
    return(NULL)
  }

  if (
    is.null(revisions$slots) ||
    is.null(revisions$slots$main)
  ) {
    return(NULL)
  }

  revisions$slots$main$`*`[1]
  # revisions[[1]]$slots$main$`*`
}


#' Fetch page HTML from MediaWiki REST API
#'
#' Downloads rendered HTML for a page using the MediaWiki REST API.
#'
#' @param page_title Character. Page title.
#' @param wiki_base Character. Base wiki URL.
#'
#' @return Character string containing HTML.
#' @export
fetch_html <- function(
    page_title,
    wiki_base = "https://www.appropedia.org"
) {

  url <- paste0(
    wiki_base,
    "/w/rest.php/v1/page/",
    utils::URLencode(
      page_title,
      reserved = TRUE
    ),
    "/html"
  )

  response <- httr2::request(url) |>
    httr2::req_perform()

  httr2::resp_body_string(response)

}



