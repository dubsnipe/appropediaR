#' Retrieve pages belonging to a category
#'
#' Retrieves the titles of pages belonging to a specified category using the
#' MediaWiki API.
#'
#' Results are automatically paginated until all category members have been
#' retrieved.
#'
#' @param category Character string containing the category name.
#' The \code{"Category:"} prefix is optional.
#' @param limit Maximum number of pages requested per API call.
#' @param namespace Namespace in numeric representation.
#' @param verbose Provide details during the run.
#'
#' @return Character vector containing page titles.
#'
#' @seealso
#' \code{\link{get_categories_for_pages}},
#' \code{\link{count_pages_in_category}}
#'
#' @examples
#' \dontrun{
#' pages <- get_pages_from_category("Water")
#' }
#'
#' @export
get_pages_from_category <- function(category,
                                    limit = 500,
                                    namespace = 0,
                                    verbose = TRUE) {

  all_pages <- character()
  cmcontinue <- NULL
  cmnamespace <- paste(namespace, collapse = "|")

  repeat {
    query <- list(
      action = "query",
      list = "categorymembers",
      cmtitle = paste0("Category:", category),
      cmlimit = limit,
      cmnamespace = cmnamespace,
      format = "json"
    )

    if (!is.null(cmcontinue)) {
      query$cmcontinue <- cmcontinue
    }

    data <- appropedia_query(query = query)
    pages <- data$query$categorymembers

    if (length(pages) > 0) {
      all_pages <- c(all_pages, pages$title)
    }

    if (verbose) { cat("Fetched total:", length(all_pages), "\n") }

    if (!is.null(data$continue$cmcontinue)) {
      cmcontinue <- data$continue$cmcontinue
    } else {
      break
    }
  }

  return(all_pages)
}
