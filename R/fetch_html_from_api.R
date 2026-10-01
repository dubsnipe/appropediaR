#' Fetch page HTML from MediaWiki Action API
#'
#' Downloads rendered HTML for a page using the MediaWiki Action API.
#'
#' @param page_title Character. Page title.
#'
#' @return Character string containing HTML.
#' @export
fetch_html_from_api <- function(page_title) {

  response <- appropedia_query(
    query = list(
      action = "parse",
      page = page_title,
      prop = "text",
      format = "json",
      formatversion = 2
    )
  )

  response$parse$text
}
