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
    
    # pages <- dat$query$categorymembers$title
    pages <- subset(dat$query$categorymembers, ns==namespace)[,3]
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
  return(cat_pages)
}

