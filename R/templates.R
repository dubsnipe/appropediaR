#' templates.r


#' Modify a template parameter in page wikitext
#'
#' Updates or inserts a parameter inside a template contained in a page's
#' wikitext.
#'
#' If the parameter already exists, its value is replaced. If the parameter is
#' not present, it is added to the first matching template instance.
#'
#' This function only modifies text locally and does not perform any API calls.
#'
#' @param content Character string containing page wikitext.
#' @param new_value Character string containing the new parameter value.
#' @param template_name Character string containing the template name.
#' @param param_name Character string containing the parameter name.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{content}: updated wikitext.
#'   \item \code{changed}: logical indicating whether a modification occurred.
#' }
#'
#' @seealso \code{\link{update_template_parameter}}
#'
#' @examples
#' \dontrun{
#' modify_template(
#'   content = page_text,
#'   new_value = "New description",
#'   template_name = "Page data",
#'   param_name = "description"
#' )
#' }
modify_template <- function(content, new_value, template_name, param_name) {
  
  original_content <- content
  
  # Escape template name for regex
  template_esc <- gsub(
    "([.|()\\^{}+$*?]|\\[|\\])",
    "\\\\\\1",
    template_name
  )
  
  # Check whether the template exists
  template_pattern <- paste0(
    "\\{\\{",
    template_esc
  )
  
  template_found <- grepl(
    template_pattern,
    content,
    perl = TRUE
  )
  
  if (!template_found) {
    return(list(
      content = original_content,
      template_found = FALSE,
      parameter_found = FALSE,
      parameter_added = FALSE,
      changed = FALSE
    ))
  }
  
  # Pattern: match the template and the parameter
  pattern <- paste0(
    "(\\{\\{",
    template_esc,
    "[^}]*?)\\|\\s*",
    param_name,
    "\\s*=\\s*([^|}]*)"
  )
  
  parameter_found <- grepl(
    pattern,
    content,
    perl = TRUE
  )
  
  if (parameter_found) {
    # Parameter exists: replace its value
    content <- gsub(
      pattern,
      paste0(
        "\\1| ",
        param_name,
        " = ",
        new_value,
        "\n"
      ),
      content,
      perl = TRUE
    )
    
    parameter_added <- FALSE
    
  } else {
    # Parameter doesn't exist: insert it before the closing of the first matching template
    insert_pattern <- paste0(
      "(\\{\\{",
      template_esc,
      "[^}]*?)}}"
    )
    
    content <- gsub(
      insert_pattern,
      paste0(
        "\\1| ",
        param_name,
        " = ",
        new_value,
        "\n}}"
      ),
      content,
      perl = TRUE
    )
    
    parameter_added <- TRUE
  }
  
  list(
    content = content,
    template_found = TRUE,
    parameter_found = parameter_found,
    parameter_added = parameter_added,
    changed = !identical(content, original_content)
  )
}


#' Update a template parameter on a wiki page
#'
#' Reads a page from the wiki, updates a template parameter, and optionally
#' saves the result through the MediaWiki API.
#'
#' This function combines page retrieval, template modification, and page
#' saving into a single workflow.
#'
#' If no change is required, the page is not saved.
#'
#' @param page_name Character string containing the page title.
#' @param template_name Character string containing the template name.
#' @param param_name Character string containing the parameter name.
#' @param new_value Character string containing the new parameter value.
#' @param session A \code{wiki_session} object returned by
#'   \code{\link{do_login}}.
#' @param dry_run Logical indicating whether the edit should be simulated
#' without saving changes.
#' @param report Logical indicating whether a structured report should be
#' returned.
#'
#' @return A list describing the operation, including page name, parameter
#' updated, timestamp, change status, and save result.
#'
#' @details
#' When \code{dry_run = TRUE}, the page is read and modified locally but no edit
#' is submitted to the wiki.
#'
#' When \code{report = TRUE}, a structured report is returned to facilitate
#' batch editing workflows and auditing.
#'
#' @seealso
#' \code{\link{modify_template}},
#' \code{\link{do_login}}
#'
#' @examples
#' \dontrun{
#' session <- do_login()
#'
#' update_template_parameter(
#'   page_name = "Water",
#'   template_name = "Page data",
#'   param_name = "description",
#'   new_value = "Example description",
#'   session = session
#' )
#' }
update_template_parameter <- function(
    page_name,
    template_name,
    param_name,
    new_value,
    session,
    dry_run = FALSE,
    report = T
) {
  
  content <- get_page_content(page_name)
  
  if (is.null(content)) {
    return(list(
      page = page_name,
      success = FALSE,
      action = "page_not_found",
      message = paste(
        "Page ",
        page_name,
        " not found"
      )
    ))
  }
  
  result <- modify_template(
    content = content,
    new_value = new_value,
    template_name = template_name,
    param_name = param_name
  )
  
  if (!result$template_found) {
    return(list(
      page = page_name,
      success = FALSE,
      action = "template_not_found",
      message = paste(
        "Template ",
        template_name,
        " not found"
      )
    ))
  }
  
  if (!result$changed) {
    return(list(
      page = page_name,
      success = TRUE,
      action = "unchanged",
      message = paste(
        "Template ",
        template_name,
        " was not changed"
      )
    ))
  }
  
  if (dry_run) {
    return(list(
      page = page_name,
      success = TRUE,
      action = if (result$parameter_added) "added" else "updated",
      content = result$content,
      message = paste(
        if (result$parameter_added) "Added " else "Updated ",
        param_name,
        " in ",
        template_name
      )
    ))
  }
  
  save_page(
    page_name = page_name,
    content = result$content,
    session = session,
    summary = paste(
      "Updating ",
      param_name,
      " in ",
      template_name
    )
  )
  
  list(
    page = page_name,
    success = TRUE,
    action = if (result$parameter_added) "added" else "updated"
  )
}