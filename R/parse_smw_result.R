#' Parse a Semantic MediaWiki result
#'
#' Converts a single Semantic MediaWiki result into a standardized tabular
#' format based on a property map.
#'
#' This function extracts values from Semantic MediaWiki printouts and maps
#' them to a predefined schema, ensuring that all expected columns are present
#' even when a page does not contain every property.
#'
#' Property values are normalized using
#' \code{\link{flatten_smw_value}}.
#'
#' @param page_result A single page result from a Semantic MediaWiki
#' \code{action=ask} query.
#' @param property_map Named character vector mapping output column names to
#' Semantic MediaWiki property names.
#'
#' @return A one-row data frame containing the parsed page metadata.
#'
#' @seealso
#' \code{\link{flatten_smw_value}},
#' \code{\link{get_semantic_query}}
#'
#' @examples
#' \dontrun{
#' page <- smw_response$query$results[[1]]
#'
#' parse_smw_result(
#'   page_result = page,
#'   property_map = property_map
#' )
#' }
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
