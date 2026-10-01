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
