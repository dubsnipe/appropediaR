#' Retrieve links for a set of pages
#'
#' Queries the MediaWiki API and returns all internal links found in the
#' supplied pages.
#'
#' @param pages_list Character vector containing page titles.
#'
#' @return A data frame with one row per page-link relationship.
#' Columns:
#' \itemize{
#'   \item source_page
#'   \item target_page
#'   \item target_namespace
#' }
#'
#' @examples
#' \dontrun{
#' get_links_for_pages(
#'   c("Energy", "Water")
#' )
#' }
#' @export
get_links_for_pages <- function(pages_list) {

  query <- list(
    action = "query",
    prop = "links",
    titles = paste(pages_list, collapse = "|"),
    pllimit = "max",
    format = "json"
  )

  res <- appropedia_query(query)

  pages <- res$query$pages

  links_df <- lapply(pages, function(page) {

    if (is.null(page$links) || nrow(page$links) == 0) {
      return(NULL)
    }

    data.frame(
      source_page = page$title,
      target_page = page$links$title,
      target_namespace = page$links$ns,
      stringsAsFactors = FALSE
    )
  })

  links_df <- Filter(Negate(is.null), links_df)

  if (length(links_df) == 0) {
    return(
      data.frame(
        source_page = character(),
        target_page = character(),
        target_namespace = integer(),
        stringsAsFactors = FALSE
      )
    )
  }

  do.call(rbind, links_df)
}
