
restrict_to_namespaces <- function(page_list, namespaces_list = c(0)){

  if(is.vector(namespaces_list) && all(namespaces_list %in% namespaces$number)){
    namespaces_tmp = namespaces[namespaces$number %in% namespaces_list, ]

    page_list = unlist(lapply(page_list, FUN = function(x) {
      gsub("^https://www.appropedia.org/", "", x)
    }))

    page_list = unlist(lapply(page_list, FUN = URLencode))

    page_list = unlist(lapply(page_list, FUN = function(x) {
      gsub("\\s", "_", x)
    }))

    for (i in 1:nrow(namespaces_tmp)){
      if (is.na(namespaces_tmp[i, "number"]) || namespaces_tmp[i, "number"] == 0) {
        # Do nothing
      } else {
        prefix_pattern = paste0(namespaces_tmp[i, "name"], ":")
        pattern_indices = grep(prefix_pattern, page_list)
        page_list = page_list[-pattern_indices]
      }
    }
    return(page_list)
  }
}

