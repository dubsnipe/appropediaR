#' Execute a Semantic MediaWiki query
#'
#' Sends a Semantic MediaWiki ask query through the MediaWiki API and returns
#' the parsed result.
#'
#' This function is a lightweight wrapper around the Semantic MediaWiki
#' \code{action=ask} API endpoint.
#'
#' @param query Character string containing a Semantic MediaWiki ask query.
#' @param handle Optional \code{httr} handle used to maintain session state
#' across requests.
#'
#' @return Parsed Semantic MediaWiki API response as a nested R list.
#'
#' @seealso
#' \code{\link{parse_smw_result}},
#' \code{\link{get_semantic_properties}}
#'
#' @examples
#' \dontrun{
#' get_semantic_query(
#'   "[[Category:Water]]|?Page title"
#' )
#' }
get_semantic_query <- function(query, handle = NULL) {
  q <- list(
    action = "ask",
    query = query,
    format = "json"
  )
  appropedia_query(q, handle)
}


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


#' Normalize a Semantic MediaWiki value
#'
#' Converts Semantic MediaWiki values into a consistent format suitable for
#' tabular storage and analysis.
#'
#' This helper handles common Semantic MediaWiki return types including page
#' references, dates, coordinates, quantities, booleans, and text values.
#'
#' Multiple values are concatenated using a semicolon separator.
#'
#' @param x A value returned by a Semantic MediaWiki printout.
#'
#' @return A normalized value suitable for inclusion in a data frame.
#' Most values are returned as character strings.
#'
#' @details
#' Supported Semantic MediaWiki value types include:
#' \itemize{
#'   \item Page references
#'   \item Dates and timestamps
#'   \item Coordinates
#'   \item Quantities and units
#'   \item Boolean values
#'   \item Wikitext links
#'   \item Plain text values
#' }
#'
#' Empty values are returned as \code{NA_character_}.
#'
#' @seealso
#' \code{\link{parse_smw_result}}
#'
#' @examples
#' \dontrun{
#' flatten_smw_value(
#'   list("Water", "Sanitation")
#' )
#' }
flatten_smw_value <- function(x) {

  # empty
  if (length(x) == 0) {
    return(NA_character_)
  }

  # page references
  if (is.data.frame(x) && "fulltext" %in% names(x)) {
    return(paste(x$fulltext, collapse = "; "))
  }

  # coordinates
  if (is.data.frame(x) &&
      all(c("lat", "lon") %in% names(x))) {
    return(
      paste(
        apply(x[, c("lat", "lon")], 1, paste, collapse = ", "),
        collapse = "; "
      )
    )
  }

  # dates (SMW datetime objects)
  if (is.data.frame(x) && "timestamp" %in% names(x)) {

    return(
      paste(
        as.character(
          as.POSIXct(
            as.numeric(as.character(x$timestamp)),
            origin = "1970-01-01",
            tz = "UTC"
          )
        ),
        collapse = "; "
      )
    )
  }

  # quantities
  if (is.data.frame(x) &&
      all(c("value", "unit") %in% names(x))) {

    return(
      paste(
        paste(x$value, x$unit),
        collapse = "; "
      )
    )
  }

  # booleans
  if (identical(x, "t")) return("TRUE")
  if (identical(x, "f")) return("FALSE")

  # wikitext (MediaWiki links like [[User:Ismii|Ismii]])
  if (is.character(x) && length(x) == 1 && grepl("\\[\\[", x[1])) {
    x <- gsub("\\[\\[(.*?)\\|(.*?)\\]\\]", "\\2", x)
    x <- gsub("\\[\\[(.*?)\\]\\]", "\\1", x)
    return(x)
  }

  # text, keywords, URIs, numbers
  paste(unlist(x), collapse = "; ")
}
