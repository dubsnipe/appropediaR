#' Extract databox templates in a page
#'
#' Extract a list of databox templates present on a page's wikitext.
#'
#' @param page_title Wiki page title
#' @param databoxes Vector containing Appropedia databox templates, e.g.,
#' {{Page data}} and {{Project data}}
#'
#' @return Data frame.
#'
#' @export
extract_templates <- function(
    page_title,
    databoxes
) {

  i_templates <- get_templates_for_pages(
    page_title
  )

  i_templates <- dplyr::filter(
    i_templates,
    template_name %in% databoxes
  )

  tibble::as_tibble(
    i_templates
  )
}
