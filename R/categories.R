#' get_categories.R


# get_page_categories <- function(page_name) {
#   
#   q <- list(action = "query",
#     prop = "categories",
#     titles = page_name,
#     format = "json"
#   )
#   
#   json_res <- appropedia_query(query = q)
#   
#   pages <- json_res$query$pages
#   page_id <- names(pages)[1]
#   
#   if (is.null(pages[[page_id]]$categories)) {
#     return(
#       return(character(0))  # no categories
#     )
#   }
#   pages[[page_id]]$categories$title
# }


#' Query the categories for a single page.
get_page_categories <- function(page_names) {
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


#' Collect the categories for a large list of pages.
collect_page_categories <- function(pages_list,
                            force_restart = FALSE,
                            checkpoint_file = "categories_checkpoint.rds",
                            chunk_size = 50,
                            checkpoint_interval = 10) {
  
  
  chunked_pages_list <- split(pages_list, ceiling(seq_along(pages_list)/chunk_size))

  checkpoint <- load_checkpoint(
    checkpoint_file,
    force_restart,
    default_value = list(
      categories_batch = vector("list", length(chunked_pages_list)),
      next_index = 1
    )
  )
  categories_batch <- checkpoint$categories_batch
  start_i <- checkpoint$next_index
  
  for (i in start_i:length(chunked_pages_list)) {
    
    chunk <- chunked_pages_list[[i]]
    
    if (length(chunk) == 0 || all(is.na(chunk))) {
      next
    }
    
    categories_batch[[i]] <- get_page_categories(chunk)
    
    next_index <- checkpoint_manager(
      i = i,
      n = length(chunked_pages_list),
      checkpoint_interval = checkpoint_interval,
      checkpoint_file = checkpoint_file,
      state = list(categories_batch = categories_batch)
    )
  }
  
  # flatten result if needed
  do.call(rbind, categories_batch)
}



count_category_pages <- function(
    category,
    limit = 500
) {
  
  offset <- 0
  total <- 0
  
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
    
    total <- total + batch_size
    
    cat(
      "Retrieved", batch_size,
      "pages. Running total:", total,
      "\n"
    )
    
    next_offset <- res[["query-continue-offset"]]
    
    if (is.null(next_offset)) {
      cat("Finished.\n")
      break
    }
    
    offset <- next_offset
  }
  
  return(total)
}



get_category_pages <- function(category,
                               limit = 500,
                               base_url = get_appropedia_api_url()) {
  
  all_pages <- character()
  cmcontinue <- NULL
  
  repeat {
    query <- list(
      action = "query",
      list = "categorymembers",
      cmtitle = paste0("Category:", category),
      cmlimit = limit,
      format = "json"
    )
    
    if (!is.null(cmcontinue)) {
      query$cmcontinue <- cmcontinue
    }
    
    res <- httr::GET(base_url, query = query)
    json <- httr::content(res, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(json)
    
    pages <- data$query$categorymembers
    
    if (length(pages) > 0) {
      all_pages <- c(all_pages, pages$title)
    }
    
    cat("Fetched", length(pages), "pages | total:", length(all_pages), "\n")
    
    if (!is.null(data$continue$cmcontinue)) {
      cmcontinue <- data$continue$cmcontinue
    } else {
      break
    }
  }
  
  return(all_pages)
}

# collect_all_categories <- function(pages_list,
#                                    start = 1,
#                                    end = length(pages_list),
#                                    save_path = "categories_temp.rds",
#                                    automatic = TRUE) {
# 
#   # Load existing progress if file exists
#   if (file.exists(save_path)) {
#     all_categories <- readRDS(save_path)
# 
#     if (automatic) {
#       # Resume from the first unprocessed page
#       start <- sum(!vapply(all_categories, is.null, logical(1))) + 1
#     }
#     cat("Resuming from row", start, "\n")
#   } else {
#     all_categories <- vector("list", length(pages_list)) # preallocate cleanly
#   }
# 
#   for (i in start:end) {
#     page_name <- pages_list[i]
#     cat(i, "-", page_name, "\n")
# 
#     page_categories <- get_all_page_categories(page_name)
# 
#     if (is.null(page_categories)) {
#       cat("Skipping:", page_name, "(No useful API response)\n")
#       all_categories[[i]] <- ""   # explicitly set empty string
#     } else {
#       page_categories_text <- paste(unlist(page_categories), collapse = ", ")
#       all_categories[[i]] <- page_categories_text
#     }
# 
#     if (i %% 100 == 0 || i == end) {
#       saveRDS(all_categories, save_path)
#     }
#   }
# 
#   # Return named vector: categories aligned with pages
#   tabulated_categories <- unlist(all_categories, use.names = FALSE)
#   names(tabulated_categories) <- pages_list
#   return(tabulated_categories)
# }
# 

# Scrape categories from the wikitext of a single page
# More expensive approach
# scrape_page_categories <- function(page){
#   tryCatch({
#     page_content <- get_page_content(page)
#   },
#   error=function(e) {
#     message('An Error Occurred')
#   }
#   )
#   if (is.null(page_content)) {
#     cat("Skipping:", page_name, "(No useful API response)\n")
#     return(NA)
#   }else{
#     raw_cats <-  page_content %>% str_extract_all("\\[\\[Category\\:(.+?)\\]\\]")
#     raw_cats <-
#       raw_cats %>%
#       lapply(FUN = function(x)
#         str_remove_all(x, pattern = "\\[\\[")) %>%
#       lapply(FUN = function(x)
#         str_remove_all(x, pattern = "\\]\\]"))
#     return(raw_cats)
#   }
# }
# # Go through a list and collect all categories
# all_scraped_categories <- function(pages_list, start = 1, end = nrow(pages_list)) {
#   for (i in start:end) {
#     cat(i, " - ")
#     page_name <- pages_list[i]
#     page_categories <- scrape_page_categories(page_name)
# 
#     if (is.null(page_categories)) {
#       cat("Skipping:", page_name, "(No useful API response)\n")
#       next
#     }
#     page_categories_text <- paste(unlist(page_categories), collapse = ", ")
#     all_categories[i] <- page_categories_text
#   }
#   return(all_categories)
# }


