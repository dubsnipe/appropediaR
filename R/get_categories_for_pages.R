#' Get categories for multiple pages
#'
#' Retrieves all categories associated with each page,
#' following MediaWiki API continuation when necessary.
#'
#' @param page_names Character vector of page titles.
#'
#' @return Data frame with columns `page` and `category`.
#' @export
get_categories_for_pages <- function(page_names) {

  stopifnot(is.character(page_names))

  if (length(page_names) == 0L) {
    return(data.frame(
      page = character(),
      category = character()
    ))
  }

  results <- list()
  continuation <- NULL

  repeat {

    q <- list(
      action = "query",
      prop = "categories",
      titles = paste(unique(page_names), collapse = "|"),
      cllimit = "max",
      format = "json"
    )

    if (!is.null(continuation)) {
      q <- c(q, continuation)
    }

    response <- appropedia_query(query = q)

    pages <- response$query$pages

    for (page in pages) {

      if (!is.null(page$categories)) {
        results[[length(results) + 1L]] <- data.frame(
          page = page$title,
          category = sub(
            "^Category:",
            "",
            page$categories$title
          )
        )
      }
    }

    continuation <- response$continue

    if (is.null(continuation)) break
  }

  if (length(results) == 0L) {
    return(data.frame(
      page = character(),
      category = character()
    ))
  }

  result <- unique(do.call(rbind, results))
  rownames(result) <- NULL

  result
}
