appropedia_query <- function(
    query,
    handle = NULL,
    base_url = get_appropedia_api_url()
) {
  
  res <- httr::GET(
    base_url,
    query = query,
    handle = handle
  )
  
  httr::stop_for_status(res)
  
  jsonlite::fromJSON(
    httr::content(res, as = "text", encoding = "UTF-8")
  )
  
}


load_checkpoint <- function(checkpoint_file, 
                            force_restart = FALSE, 
                            default_value = NULL) {
  
  if (force_restart && file.exists(checkpoint_file)) {
    file.remove(checkpoint_file)
    message("Existing checkpoint removed. Starting from scratch.")
  }
  
  if (file.exists(checkpoint_file)) {
    return(readRDS(checkpoint_file))
  }
  
  default_value
}


checkpoint_manager <- function(i,
                               n,
                               checkpoint_interval,
                               checkpoint_file,
                               state,
                               label = "Checkpoint",
                               state_keys = c("resolved", "next_index")) {
  
  next_index <- i + 1
  
  if (i %% checkpoint_interval == 0 || i == n) {
    next_index <- if (i < n) i + 1 else i
    # saveRDS(
    #   list(
    #     resolved = state$resolved,
    #     next_index = next_index
    #   ),
    #   checkpoint_file
    # )
    saveRDS(
      list(
        results = state[[state_keys[1]]],
        next_index = state[[state_keys[2]]]
      ),
      checkpoint_file
    )
    cat(label, "saved at row", i, "\n")
  }
  next_index
}
# Future recommendation: split checkpoint system into:
#   
# checkpoint_vector_state()   # redirects, linear processing
# checkpoint_cursor_state()   # SMW, pagination

#' Debug function
build_smw_query <- function(category, property_map) {
  
  props <- names(property_map)
  props <- props[!is.na(props) & props != ""]
  
  property_clause <- paste0("|?", paste(props, collapse = "|?"))
  
  query <- paste0(
    "[[Category:", category, "]]",
    property_clause
  )
  
  return(query)
}