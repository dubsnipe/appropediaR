#' Retrieve all categories on the wiki
#'
#' Queries the MediaWiki API and returns all categories, optionally filtered
#' by minimum membership size.
#'
#' @param limit Number of categories retrieved per API request.
#' @param min_members Minimum number of members required for a category to be
#' returned.
#' @param handle Optional httr handle.
#' @param verbose Provide details during the run.
#'
#' @return A data frame containing category information.
#'
#' @examples
#' \dontrun{
#' categories <- get_all_categories()
#' }
#' @export
get_all_categories <- function(
    limit = 500,
    min_members = 10,
    handle = NULL,
    verbose = TRUE
) {

  all_categories <- list()
  cont <- NULL

  repeat {

    q <- list(
      action = "query",
      list = "allcategories",
      aclimit = limit,
      acmin = min_members,
      acprop = "size",
      format = "json"
    )

    if (!is.null(cont)) {
      q <- c(q, cont)
    }

    dat <- appropedia_query(
      query = q,
      handle = handle
    )

    categories <- dat$query$allcategories

    if (!is.null(categories) && nrow(categories) > 0) {

      names(categories)[names(categories) == "*"] <- "category"

      all_categories[[length(all_categories) + 1]] <- categories
    }

    current_total <- sum(vapply(all_categories, nrow, integer(1)))

    if (verbose) {
      cat(
        "Fetched total:",
        current_total,
        "categories\n"
      )
    }

    if (!is.null(dat$continue)) {
      cont <- dat$continue
    } else {
      break
    }
  }

  if (length(all_categories) == 0) {
    return(
      data.frame(
        category = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  do.call(rbind, all_categories)
}
