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
