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
#'
#' @export
modify_template <- function(content, new_value, template_name, param_name) {

  if (is.null(content) || length(content) == 0) {
    stop("content is NULL or empty")
  }

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
