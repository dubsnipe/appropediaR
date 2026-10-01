#' Retrieve semantic properties for a collection of pages
#'
#' Collects Semantic MediaWiki properties for a large list of pages.
#'
#' Pages are queried in batches and results are converted into a standardized
#' tabular structure using the supplied property map.
#'
#' Progress is periodically saved to a checkpoint file so that interrupted
#' workflows can be resumed without restarting from the beginning.
#'
#' @param pages_list Character vector containing page titles.
#' @param property_map Named character vector mapping output column names to
#' Semantic MediaWiki property names.
#' @param handle Optional \code{httr} handle used to maintain session state
#' across requests.
#' @param force_restart Logical indicating whether an existing checkpoint
#' should be deleted and processing restarted.
#' @param checkpoint_file Character string containing the checkpoint filename
#' or file path.
#' @param chunk_size Number of pages included in each Semantic MediaWiki query.
#' @param checkpoint_interval Number of chunks processed between checkpoint
#' saves.
#' @param verbose Progress prints while it runs.
#'
#' @return A data frame containing one row per page and one column per property
#' defined in \code{property_map}.
#'
#' @details
#' This function is designed for large-scale metadata extraction workflows.
#'
#' The output schema is determined by \code{property_map}, ensuring that the
#' same columns are returned even when pages contain different subsets of
#' properties.
#'
#' Checkpoints are automatically cleaned up after successful completion unless
#' processing is interrupted.
#'
#' @seealso
#' \code{\link{get_semantic_query}},
#' \code{\link{parse_smw_result}},
#' \code{\link{property_map}}
#'
#' @examples
#' \dontrun{
#' pages <- get_pages_from_category("Water")
#'
#' metadata <- get_semantic_properties(
#'   pages_list = pages,
#'   property_map = property_map
#' )
#' }
#'
#' @export
get_semantic_properties <- function(
    pages_list,
    property_map,
    handle = NULL,
    force_restart = FALSE,
    checkpoint_file = "semantic_properties_checkpoint.rds",
    chunk_size = 10,
    checkpoint_interval = 5,
    verbose = FALSE
) {

  chunked_pages_list <- split(
    pages_list,
    ceiling(seq_along(pages_list) / chunk_size)
  )

  checkpoint <- load_checkpoint(
    checkpoint_file,
    force_restart,
    default_value = list(
      results_batch = vector(
        "list",
        length(chunked_pages_list)
      ),
      next_index = 1
    ),
    input_length = length(pages_list)
  )


  results_batch <- checkpoint$results_batch
  start_i <- checkpoint$next_index

  property_clause <- paste0(
    "|?",
    paste(names(property_map), collapse = "|?")
  )

  for (i in start_i:length(chunked_pages_list)) {

    chunk <- chunked_pages_list[[i]]

    if(verbose){
      cat("Processing chunk", i, "of", length(chunked_pages_list), "\n" )
    }

    page_clause <- paste0("[[", chunk, "]]", collapse = " OR ")

    query <- paste0(
      page_clause,
      property_clause
    )

    smw_response <- get_semantic_query(
      query,
      handle
    )

    pages <- smw_response$query$results

    results_batch[[i]] <- dplyr::bind_rows(
      lapply(
        pages,
        function(page) {
          parse_smw_result(
            page,
            property_map
          )
        }
      )
    )

    checkpoint_manager(
      i = i,
      input_size = length(chunked_pages_list),
      checkpoint_interval = checkpoint_interval,
      checkpoint_file = checkpoint_file,
      state = list(
        results_batch = results_batch
      )
    )
  }

  dplyr::bind_rows(results_batch)
}
