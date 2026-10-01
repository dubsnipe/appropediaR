#' Extract explicit categories from page wikitext.
#'
#' Identifies category declarations that are explicitly present in page
#' content and returns their category names.
#'
#' Category names are extracted from MediaWiki category tags such as:
#'
#' \preformatted{
#' [[Category:Water]]
#' [[Category:Projects]]
#' }
#'
#' Category declarations are matched case-insensitively, so the following
#' are treated equivalently:
#'
#' \preformatted{
#' [[Category:Water]]
#' [[category:Water]]
#' [[CATEGORY:Water]]
#' }
#'
#' Categories added through template transclusion are not detected because
#' they do not exist directly in the page wikitext.
#'
#' @param content Character string containing page wikitext.
#'
#' @return A character vector containing explicit category names. Returns
#' an empty character vector if no explicit categories are found.
#'
#' Category sort keys are not currently preserved. For example:
#'
#' \preformatted{
#' [[Category:Projects|Solar distiller]]
#' }
#'
#' will return "Projects".
#'
#' @examples
#' \dontrun{
#' content <- "
#' {{Page data}}
#'
#' [[Category:Water]]
#' [[Category:Projects]]
#' "
#'
#' extract_explicit_categories(content)
#' }
#'
#' @export
extract_explicit_categories <- function(content) {

  matches <- stringr::str_match_all(
    content,
    "\\[\\[(?i:category):([^\\]|]+)"
  )[[1]]

  if (nrow(matches) == 0) {
    return(character())
  }

  unique(trimws(matches[, 2]))
}
