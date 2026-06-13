#' templates.r


# edit_page <- function(page_name,
#                       new_content,
#                       session,
#                       param_name,
#                       max_retries = 5) {
#   retry_delay <- 5
#   for (attempt in seq_len(max_retries)) {
#     res <- POST(
#       get_appropedia_api_url(),
#       body = list(
#         action  = "edit",
#         title   = page_name,
#         text    = new_content,
#         summary = paste0("Updating ", param_name),
#         token   = session$csrf_token,
#         format  = "json",
#         bot = 1
#       ),
#       encode = "form",
#       handle = session$handle
#     )
#     response <- content(res, as = "parsed", type = "application/json")
# 
#     if (!is.null(response$edit) && response$edit$result == "Success") {
#       cat("Edited:", page_name, "\n")
#       return(TRUE)
#     }
# 
#     cat("Edit failed for", page_name, "\n")
#     str(response)
#     return(FALSE)
# 
# 
#     if (!is.null(response$edit) && response$edit$result == "Success") {
#       return(TRUE)
#     }
# 
#     if (!is.null(response$error) && response$error$code == "ratelimited") {
#       Sys.sleep(retry_delay)
#       retry_delay <- min(retry_delay * 2, 60)
#     } else {
#       return(FALSE)
#     }
#   }
#   FALSE
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



update_template_parameter <- function(
    page_name,
    template_name,
    param_name,
    new_value,
    session
) {
  
  content <- get_page_content(page_name)
  
  updated_content <- modify_template(
    content = content,
    new_value = new_value,
    template_name = template_name,
    param_name = param_name
  )
  
  save_page(
    page_name = page_name,
    content = updated_content,
    session = session,
    summary = paste(
      "Updating",
      param_name,
      "in",
      template_name
    )
  )
}
