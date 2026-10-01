#' Retrieve templates for a set of pages
#'
#' Queries the MediaWiki API and returns all templates transcluded in the
#' supplied pages.
#'
#' @param pages_list Character vector containing page titles.
#'
#' @return A data frame with one row per page-template relationship.
#'
#' @examples
#' \dontrun{
#' get_templates_for_pages(
#'   c("Water", "Energy")
#' )
#' }
#' @export
get_templates_for_pages <- function(pages_list) {

  query <- list(
    action = "query",
    prop = "templates",
    titles = paste(pages_list, collapse = "|"),
    tllimit = "max",
    format = "json"
  )

  res <- appropedia_query(query)

  pages <- res$query$pages

  templates_df <- lapply(pages, function(page) {

    if (is.null(page$templates) || nrow(page$templates) == 0) {
      return(NULL)
    }

    data.frame(
      source_page = page$title,
      template_name = sub(
        "^Template:",
        "",
        page$templates$title
      ),
      stringsAsFactors = FALSE
    )
  })

  templates_df <- Filter(Negate(is.null), templates_df)

  if (length(templates_df) == 0) {
    return(
      data.frame(
        source_page = character(),
        template_name = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  do.call(rbind, templates_df)
}
