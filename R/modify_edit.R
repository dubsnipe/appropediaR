

#' Read a page's wikitext
#'
#'
#' Anonymous function
get_page_content <- function(page_name) {
  tryCatch({
    res <- GET(get_appropedia_api_url(), query = list(
      action = "query",
      prop = "revisions",
      rvslots = "main",
      rvprop = "content",
      titles = page_name,
      format = "json"
    ), timeout(5))

    pages <- content(res, as = "parsed", type = "application/json")$query$pages
    page_id <- names(pages)[1]
    if (!is.null(pages[[page_id]]$revisions)) {
      return(pages[[page_id]]$revisions[[1]]$slots$main$`*`)
    } else {
      return(NULL)  # Explicitly return NULL for missing wikitext
    }
  }, error = function(e) {
    return(NULL)
  })
}


#' Edit a page
#'
#'
#' Debug
edit_page <- function(page_name,
                      new_content,
                      session,
                      param_name,
                      max_retries = 5) {
  retry_delay <- 5
  for (attempt in seq_len(max_retries)) {
    res <- POST(
      get_appropedia_api_url(),
      body = list(
        action  = "edit",
        title   = page_name,
        text    = new_content,
        summary = paste0("Updating ", param_name),
        token   = session$csrf_token,
        format  = "json",
        bot = 1
      ),
      encode = "form",
      handle = session$handle
    )
    response <- content(res, as = "parsed", type = "application/json")

    if (!is.null(response$edit) && response$edit$result == "Success") {
      cat("Edited:", page_name, "\n")
      return(TRUE)
    }

    cat("Edit failed for", page_name, "\n")
    str(response)
    return(FALSE)


    if (!is.null(response$edit) && response$edit$result == "Success") {
      return(TRUE)
    }

    if (!is.null(response$error) && response$error$code == "ratelimited") {
      Sys.sleep(retry_delay)
      retry_delay <- min(retry_delay * 2, 60)
    } else {
      return(FALSE)
    }
  }
  FALSE
}


# edit_page <- function(page_name, new_content, max_retries = 5, param_to_modify, handle) {
#   retry_delay <- 5
#   for (attempt in 1:max_retries) {
#     res <- POST(config$api_url, body = list(
#       action = "edit",
#       title = page_name,
#       text = new_content,
#       summary = paste0("Updating ", param_to_modify),
#       token = session$csrf_token,
#       format = "json"
#     ), encode = "form")
#
#     response <- content(res, as = "parsed", type = "application/json")
#
#     if (!is.null(response$edit) && response$edit$result == "Success") {
#       cat("Successfully edited:", page_name, "\n")
#       return(TRUE)
#     }
#
#     if (!is.null(response$error) && response$error$code == "ratelimited") {
#       cat("Rate limit hit. Retrying in", retry_delay, "seconds...\n")
#       Sys.sleep(retry_delay)
#       retry_delay <- min(retry_delay * 2, 60)
#     } else {
#       cat("Failed to edit:", page_name, "- Response:", jsonlite::toJSON(response, pretty = TRUE), "\n")
#       return(FALSE)
#     }
#   }
#   cat("Max retries reached. Skipping:", page_name, "\n")
#   return(FALSE)
# }


#' Modify a specific page's template using regex on the wikitext.
#'
#'
#' Debug
modify_template <- function(content, new_value, template_name, param_name) {
  # Escape template name for regex
  template_esc <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", template_name)

  # Pattern: match the template and the parameter
  pattern <- paste0("(\\{\\{", template_esc, "[^}]*?)\\|\\s*", param_name, "\\s*=\\s*([^|}]*)")

  if (grepl(pattern, content, perl = TRUE)) {
    # Parameter exists: replace its value
    content <- gsub(pattern,
                    paste0("\\1| ", param_name, " = ", new_value, "\n"),
                    content,
                    perl = TRUE)
  } else {
    # Parameter doesn't exist: insert it before the closing of the first matching template
    insert_pattern <- paste0("(\\{\\{", template_esc, "[^}]*?)}}")
    content <- gsub(insert_pattern,
                    paste0("\\1| ", param_name, " = ", new_value, "\n}}"),
                    content,
                    perl = TRUE)
  }
  return(content)
}


#' Modify a specific page's template using regex on the wikitext.
#'
#'
#' Debug
# update_pages <- function(df, handle, csrf_token,
#                          template_name, page_col, value_col, param_name) {
#
#   for (i in seq_len(nrow(df))) {
#     page_name <- df[[page_col]][i]
#     new_param_value <- df[[value_col]][i]
#
#     page_content <- get_page_content(page_name)
#     if (is.null(page_content)) {
#       cat("Skipping:", page_name, "(No wikitext found)\n")
#       next
#     }
#
#     new_content <- modify_template(
#       content = page_content,
#       new_value = new_param_value,
#       template_name = template_name,
#       param_name = param_name
#     )
#
#     if (identical(new_content, page_content)) {
#       cat("Skipping:", page_name, "(No change needed)\n")
#       next
#     }
#
#     edit_page(
#       page_name,
#       new_content = new_content,
#       handle,
#       csrf_token,
#       param_name
#     )
#
#     Sys.sleep(1)
#   }
# }
# update_pages <- function(df, session = session, template_name, page_col, value_col, param_name) {
#   for (i in seq_len(nrow(df))) {
#     page_name <- df[[page_col]][i]
#     new_param_value <- df[[value_col]][i]
#
#     page_content <- get_page_content(page_name)  # your existing function
#     if (is.null(page_content)) {
#       cat("Skipping:", page_name, "(No wikitext found)\n")
#       next
#     }
#
#     new_content <- modify_template(content = page_content,
#                                    new_value = new_param_value,
#                                    template_name = template_name,
#                                    param_name = param_name)
#     if (new_content == page_content) {
#       cat("Skipping:", page_name, "(No change needed)\n")
#       next
#     }
#
#     edit_page(page_name, new_content, session, param_to_modify = param_name)
#     Sys.sleep(1)  # optional delay
#   }
# }

