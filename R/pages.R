#' get_pages.R


#' Get a full list of pages on Appropedia.
#' This function retrieves pages from using a MediaWiki API query.
#' The retrieved page list will contain redirects. To clean the list, use
#' apply_redirects().
get_all_pages <- function(base_url = get_appropedia_api_url(), 
                          limit = 500, 
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

    # res <- GET(base_url, query = q, handle = handle)
    # dat <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

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


#' Get a list of pages inside a category.
#' This function will list only from a category.
get_category_pages <- function(
    category,
    base_url = get_appropedia_api_url(),
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
    
    res <- GET(base_url, query = q, handle = handle)
    dat <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
    
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

