#' Retrieve category memberships for one or more pages
#'
#' Queries the MediaWiki API and returns the categories assigned to one or more
#' pages.
#'
#' Multiple page titles may be supplied in a single request, allowing category
#' information to be collected efficiently in batches.
#'
#' @param page_names Character vector containing one or more page titles.
#'
#' @return A data frame with one row per page-category relationship and the
#' following columns:
#' \itemize{
#'   \item \code{page}: page title.
#'   \item \code{category}: category name.
#' }
#'
#' Category names are returned without the \code{"Category:"} prefix.
#'
#' @seealso
#' \code{\link{batch_get_page_categories}},
#' \code{\link{get_pages_from_category}}
#'
#' @examples
#' \dontrun{
#' get_categories_for_pages("Water")
#'
#' get_categories_for_pages(
#'   c("Water", "Ocean")
#' )
#' }
#'
#' @export
get_categories_for_pages <- function(page_names) {
  q <- list(action = "query",
            prop = "categories",
            titles = paste(page_names, collapse = "|"),
            format = "json"
  )

  json_res <- appropedia_query(query = q)
  pages <- json_res$query$pages

  result <- lapply(
    pages,
    function(page) {
      if (is.null(page$categories)){
        return(
          data.frame(
            page = page$title,
            category = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      }

      data.frame(
        page = page$title,
        category = sub("^Category:", "", page$categories$title),
        stringsAsFactors = FALSE
      )
    }
  )

  df <- do.call(rbind, result)
  rownames(df) <- NULL
  df

}
