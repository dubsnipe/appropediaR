#' Extract text from HTML
#'
#' Extract text from an HTML document using different strategies.
#'
#' @param html Character string containing HTML.
#' @param strategy Extraction strategy.
#' @param remove_selectors Character vector of CSS selectors to remove.
#' @param n Number of characters for excerpt strategies.
#'
#' @return Character string.
#'
#' @export
extract_text <- function(
    html,
    strategy = c(
      "lead",
      "all",
      "first_n_chars"
    ),
    remove_selectors = DEFAULT_REMOVE_SELECTORS,
    n = DEFAULT_EXCERPT_SIZE
) {

  strategy <- match.arg(strategy)

  doc <- xml2::read_html(html)

  if (length(remove_selectors) > 0) {

    selector_string <- paste(
      remove_selectors,
      collapse = ", "
    )

    elements <- rvest::html_elements(
      doc,
      selector_string
    )

    xml2::xml_remove(
      elements
    )
  }

  paragraphs <- rvest::html_elements(
    doc,
    "p"
  )

  paragraphs <- rvest::html_text2(
    paragraphs
  )

  paragraphs <- gsub(
    "[[:space:]]+",
    " ",
    paragraphs
  )

  paragraphs <- trimws(
    paragraphs
  )

  paragraphs <- paragraphs[
    nzchar(paragraphs)
  ]

  if (length(paragraphs) == 0) {
    return(NA_character_)
  }

  if (strategy == "lead") {
    return(paragraphs[[1]])
  }

  full_text <- paste(
    paragraphs,
    collapse =  " "
  )

  if (strategy == "all") {
    return(full_text)
  }

  substr(
    full_text,
    1,
    n
  )
}

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
