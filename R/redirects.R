#' sort_redirects.R


#' Get the immediate redirect page for a single page.
#' This function will follow the immediate redirect to which a page points.
#' This helper function is chained by resolve_redirect() to find the final
#' page to which a single page points.
get_redirect_url <- function(page_name, 
                             api_url = get_appropedia_api_url()) {
  tryCatch({
    res <- GET(api_url, query = list(
      action = "query",
      prop = "info",
      redirects="",
      titles = page_name,
      format = "json"
    ), timeout(5))
    
    pages <- content(res, as = "parsed", 
                     type = "application/json"
    )
    
    if (!is.null(pages$query$redirects[[1]]$to)) {
      response <- pages$query$redirects[[1]]$to %>% str_replace_all(" ", "_")
      return(response)
    } else {
      return(NULL)  # Explicitly return NULL for missing wikitext
    }
  })
}


#' Starting with a page, follow redirects until it finds a page.
#' 
resolve_redirect <- function(page_name, seen = character()) {
  
  # Prevent infinite loops if there's a redirect cycle
  if (page_name %in% seen) {
    return(page_name)
  }
  
  redirect_url <- get_redirect_url(page_name)
  
  if (!is.null(redirect_url) && nchar(redirect_url) > 0) {
    # Follow the redirect recursively
    return(resolve_redirect(redirect_url, c(seen, page_name)))
  } else {
    # No redirect, return final destination
    return(page_name)  
  }
}


#' Resolve redirects for a list of pages.
#' Use this function to clean a pages list. This function stores a 
apply_redirects <- function(names_list,
                            force_restart = FALSE,
                            checkpoint_file = "redirects_checkpoint.rds",
                            checkpoint_interval = 100) {
  
  
  checkpoint <- load_checkpoint(
    checkpoint_file,
    force_restart,
    default_value = list(
      resolved = character(length(names_list)),
      next_index = 1
    )
  )
  
  resolved <- checkpoint$resolved
  start_i <- checkpoint$next_index
  
  for (i in start_i:length(names_list)) {
    cat("Resolving redirects for row", i, ":", names_list[i], "\n")
    
    # Handle empty or NA entries
    if (is.na(names_list[i]) || names_list[i] == "") {
      cat(" → Empty row, skipping\n")
      resolved[i] <- names_list[i]
      next
    }
    
    final_target <- resolve_redirect(names_list[i])
    if (final_target != names_list[i]) {
      cat(" → Redirect resolved to:", final_target, "\n")
      resolved[i] <- final_target
    } else {
      cat(" → No redirect\n")
      resolved[i] <- names_list[i]
    }
    
    next_index <- checkpoint_manager(
      i = i,
      n = length(names_list),
      checkpoint_interval = checkpoint_interval,
      checkpoint_file = checkpoint_file,
      state = list(resolved = resolved)
    )
  }
  
  cat("Redirect resolution complete. Total rows processed:", length(names_list), "\n")
  cat("Original pages list size:", length(names_list), "\n")
  return(resolved)
}