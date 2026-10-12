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
