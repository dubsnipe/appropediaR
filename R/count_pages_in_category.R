#' Count pages in a category
#'
#' Counts the number of pages belonging to a category using the MediaWiki API.
#'
#' This function can be useful for estimating workflow size before retrieving
#' page metadata or category memberships.
#'
#' @param category Character string containing the category name.
#' The \code{"Category:"} prefix is not required.
#' @param limit Maximum number of category members requested per API call.
#'
#' @return Integer containing the number of pages found in the category.
#'
#' @seealso
#' \code{\link{get_category_pages}}
#'
#' @examples
#' \dontrun{
#' count_pages_in_category("Water")
#' }
#'
#' @export
count_pages_in_category <- function(
    category,
    limit = 500
) {

  offset <- 0
  processed_total  <- 0

  repeat {

    query <- paste(
      c(
        paste0("[[Category:", category, "]]"),
        "limit=500",
        paste0("offset=", offset)
      ),
      collapse = "|"
    )

    cat("Querying offset", offset, "\n")

    res <- get_semantic_query(query)

    batch_size <- length(res$query$results)

    processed_total  <- processed_total + batch_size

    cat(
      "Retrieved", batch_size,
      "pages. Running total:", processed_total,
      "\n"
    )

    next_offset <- res[["query-continue-offset"]]

    if (is.null(next_offset)) {
      cat("Finished.\n")
      break
    }

    offset <- next_offset
  }

  return(processed_total)
}
