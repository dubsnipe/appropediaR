#' get_pages.R


#' Retrieve pages from a MediaWiki namespace
#'
#' Retrieves page titles from Appropedia using the MediaWiki
#' \code{allpages} API query.
#'
#' Results are automatically paginated using the API continuation mechanism
#' until all matching pages have been retrieved.
#'
#' @param limit Maximum number of pages requested per API call.
#' The MediaWiki API may impose its own limits depending on user rights.
#' @param handle Optional \code{httr} handle used to maintain session state
#' across requests.
#' @param namespace Integer namespace identifier. Defaults to \code{0}
#' (main namespace).
#'
#' Use \code{mediawiki_namespaces} to identify namespace numbers.
#'
#' @return A character vector containing page titles.
#'
#' @seealso
#' \code{\link{mediawiki_namespaces}}
#'
#' @examples
#' \dontrun{
#' # Retrieve all pages in the main namespace
#' pages <- get_all_pages()
#'
#' # Retrieve all templates
#' templates <- get_all_pages(namespace = 10)
#' }
#'
#' @export
get_all_pages <- function(limit = 500,
                          handle = NULL,
                          namespace = 0) {
  all_pages <- c()
  cont <- NULL
  repeat {
    q <- list(
      action = "query",
      list   = "allpages",
      aplimit = limit,
      apnamespace = namespace,
      format = "json"
    )
    if (!is.null(cont)) q <- c(q, cont)

    dat <- appropedia_query(
      query = q,
      handle = handle
    )

    if (!is.null(dat$error)) {
      stop(paste("API error:", dat$error$info))
    }

    pages <- dat$query$allpages$title
    all_pages <- c(all_pages, pages)

    if (!is.null(dat$continue)) {
      cont <- dat$continue
    } else {
      break
    }
  }
  if(namespace == 0){
    namespace_name = "Main"
  } else {
    namespace_name = mediawiki_namespaces[namespace,]["name"]
  }

  message = paste ("Returned", length(all_pages), "pages from namespace:", namespace_name, "\n")
  cat(message)
  return(all_pages)
}



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


#' Retrieve pages from a category
#'
#' Retrieves the titles of pages belonging to a specified category using the
#' MediaWiki API.
#'
#' Results are automatically paginated using the API continuation mechanism
#' until all matching pages have been retrieved.
#'
#' @param category Character string containing the category name.
#' The category name must be stripped of the \code{"Category:"} prefix.
#' @param limit Maximum number of pages requested per API call.
#' The MediaWiki API may impose its own limits depending on user rights.
#' @param handle Optional \code{httr} handle used to maintain session state
#' across requests.
#' @param namespace Integer namespace identifier used to filter results.
#' Defaults to \code{0} (main namespace).
#'
#' @return A character vector containing page titles.
#'
#' @seealso
#' \code{\link{get_all_pages}},
#' \code{\link{mediawiki_namespaces}}
#'
#' @examples
#' \dontrun{
#' # Retrieve all pages in Category:Water
#' pages <- get_category_pages("Water")
#'
#' # Retrieve all pages in Category:Projects
#' projects <- get_category_pages("Projects")
#' }
#'
#' @export
get_category_pages <- function(
    category,
    limit = 500,
    handle = NULL,
    namespace = 0
) {

  cat_pages <- c()
  cont <- NULL

  repeat {

    q <- list(
      action = "query",
      list = "categorymembers",
      cmtitle = paste0("Category:", category),
      cmlimit = limit,
      format = "json"
    )

    if (!is.null(cont)) q <- c(q, cont)

    dat <- appropedia_query(
      q,
      handle = handle
    )

    if (!is.null(dat$error)) {
      stop(paste("API error:", dat$error$info))
    }

    pages <-
      dat$query$categorymembers[dat$query$categorymembers$ns == namespace, ]
    #  subset(dat$query$categorymembers, ns==namespace)[,3] substituted
    cat_pages <- c(cat_pages, pages)

    if (!is.null(dat$continue)) {
      cont <- dat$continue
    } else {
      break
    }
  }
  if(namespace == 0){
    namespace_name = "Main"
  } else {
    namespace_name = mediawiki_namespaces[namespace,]["name"]
  }

  message = paste ("Returned", length(cat_pages),
                   "pages from namespace:",
                   namespace_name, "\n")
  cat(message)
  return(cat_pages$title)
}


#' Retrieve subcategories for a category
#'
#' @param category Category name without the "Category:" prefix.
#'
#' @return A data frame containing parent-child category relationships.
#'
#' @export
get_category_subcategories <- function(category) {

  subcategories <- get_pages_from_category(
    category = category,
    namespace = 14
  )

  if (length(subcategories) == 0) {
    return(
      data.frame(
        parent_category = character(),
        child_category = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  data.frame(
    parent_category = category,
    child_category = sub("^Category:", "", subcategories),
    stringsAsFactors = FALSE
  )
}


#' Build Appropedia's category tree as a dictionary
#'
#' @return A database that pairs parent and child categories.
#'
#' @export
get_category_tree <- function() {

  categories <- get_all_categories(verbose = FALSE)

  categories_with_children <- categories$category[
    categories$subcats > 0
  ]

  tree <- lapply(
    seq_along(categories_with_children),
    function(i) {

      cat(
        i, "/",
        length(categories_with_children),
        ":",
        categories_with_children[i],
        "\n"
      )

      get_category_subcategories(
        categories_with_children[i]
      )
    }
  )

  do.call(rbind, tree)
}
