
#' Read a page's wikitext
#'
get_page_content <- function(
    page_name,
    handle = NULL
) {
  
  if (!is.null(session)) {
    stop_if_invalid_session(session)
    handle <- session$handle
  }
  
  q <- list(
    action = "query",
    prop = "revisions",
    rvslots = "main",
    rvprop = "content",
    titles = page_name,
    format = "json"
  )
  
  res <- tryCatch(
    appropedia_query(
      q,
      handle = handle
    ),
    error = function(e) {
      return(NULL)
    }
  )
  
  if (is.null(res)) {
    return(NULL)
  }
  
  pages <- res$query$pages
  page_id <- names(pages)[1]
  revisions <- pages[[page_id]]$revisions
  
  if (is.null(revisions)) {
    return(NULL)
  }
  
  if (
    is.null(revisions$slots) ||
    is.null(revisions$slots$main)
  ) {
    return(NULL)
  }
  
  revisions$slots$main$`*`[1]
}

