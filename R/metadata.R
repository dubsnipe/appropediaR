#' metadata.R

#' Single interface to SMW API
get_semantic_query <- function(query, handle = NULL) {
  q <- list(
    action = "ask",
    query = query,
    format = "json"
  )
  appropedia_query(q, handle)
}

parse_smw_result <- function(page_result, property_map) {
  
  # start from full schema (ensures all NA columns exist)
  out <- as.list(property_map)
  
  # title (always present)
  out$Title <- page_result$fulltext %||% NA_character_
  
  # fill SMW properties
  for (col_name in names(property_map)) {
    
    smw_key <- property_map[[col_name]]
    
    value <- page_result$printouts[[smw_key]]
    if (is.null(value)) value <- list()
    
    out[[col_name]] <- flatten_smw_value(value)
  }
  
  as.data.frame(out, stringsAsFactors = FALSE)
}


#' Flatten a Semantic MediaWiki result.
#' This function normalizes SMW return types
flatten_smw_value <- function(x) {
  
  # empty
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  # page references
  if (is.data.frame(x) && "fulltext" %in% names(x)) {
    return(paste(x$fulltext, collapse = "; "))
  }
  
  # coordinates
  if (is.data.frame(x) &&
      all(c("lat", "lon") %in% names(x))) {
    return(
      paste(
        apply(x[, c("lat", "lon")], 1, paste, collapse = ", "),
        collapse = "; "
      )
    )
  }
  
  # dates (SMW datetime objects)
  if (is.data.frame(x) && "timestamp" %in% names(x)) {
    
    return(
      paste(
        as.character(
          as.POSIXct(
            as.numeric(as.character(x$timestamp)),
            origin = "1970-01-01",
            tz = "UTC"
          )
        ),
        collapse = "; "
      )
    )
  }
  
  # quantities
  if (is.data.frame(x) &&
      all(c("value", "unit") %in% names(x))) {
    
    return(
      paste(
        paste(x$value, x$unit),
        collapse = "; "
      )
    )
  }
  
  # booleans
  if (identical(x, "t")) return("TRUE")
  if (identical(x, "f")) return("FALSE")
  
  # wikitext (MediaWiki links like [[User:Ismii|Ismii]])
  if (is.character(x) && length(x) == 1 && grepl("\\[\\[", x[1])) {
    x <- gsub("\\[\\[(.*?)\\|(.*?)\\]\\]", "\\2", x)
    x <- gsub("\\[\\[(.*?)\\]\\]", "\\1", x)
    return(x)
  }
  
  # text, keywords, URIs, numbers
  paste(unlist(x), collapse = "; ")
}


#' Collect semantic properties for a list of pages.
get_semantic_properties <- function(
    pages_list,
    property_map,
    handle = NULL,
    force_restart = FALSE,
    checkpoint_file = "semantic_properties_checkpoint.rds",
    chunk_size = 20,
    checkpoint_interval = 5
) {
  
  chunked_pages_list <- split(
    pages_list,
    ceiling(seq_along(pages_list) / chunk_size)
  )
  
  checkpoint <- load_checkpoint(
    checkpoint_file,
    force_restart,
    default_value = list(
      results_batch = vector("list", length(chunked_pages_list)),
      next_index = 1
    )
  )
  
  results_batch <- checkpoint$results_batch
  start_i <- checkpoint$next_index
  
  property_clause <- paste0(
    "|?",
    paste(names(property_map), collapse = "|?")
  )
  
  for (i in start_i:length(chunked_pages_list)) {
    
    chunk <- chunked_pages_list[[i]]
    
    cat(
      "Processing chunk",
      i,
      "of",
      length(chunked_pages_list),
      "\n"
    )
    
    page_clause <- paste0(
      "[[",
      chunk,
      "]]",
      collapse = " OR "
    )
    
    query <- paste0(
      page_clause,
      property_clause
    )
    
    smw_response <- get_semantic_query(
      query,
      handle
    )
    
    pages <- smw_response$query$results
    
    results_batch[[i]] <- dplyr::bind_rows(
      lapply(
        pages,
        function(page) {
          parse_smw_result(
            page,
            property_map
          )
        }
      )
    )
    
    checkpoint_manager(
      i = i,
      n = length(chunked_pages_list),
      checkpoint_interval = checkpoint_interval,
      checkpoint_file = checkpoint_file,
      state = list(
        results_batch = results_batch
      )
    )
  }
  
  dplyr::bind_rows(results_batch)
}