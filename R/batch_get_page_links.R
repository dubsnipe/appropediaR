#' Retrieve links for a large list of pages
#'
#' Retrieves internal page links for a large collection of pages by querying
#' the MediaWiki API in batches.
#'
#' This function automatically chunks requests, periodically saves progress to
#' a checkpoint file, and can resume interrupted workflows.
#'
#' @param pages_list Character vector containing page titles.
#' @param force_restart Logical indicating whether an existing checkpoint
#' should be deleted and processing restarted.
#' @param checkpoint_file Character string containing the checkpoint filename
#' or file path.
#' @param chunk_size Number of page titles to include in each API request.
#' @param checkpoint_interval Number of chunks processed between checkpoint
#' saves.
#' @param delete_temp Logical indicating whether the checkpoint file should be
#' removed after successful completion.
#'
#' @return A data frame with one row per page-link relationship.
#'
#' @seealso
#' \code{\link{get_links_for_pages}},
#' \code{\link{checkpoint_manager}},
#' \code{\link{load_checkpoint}}
#'
#' @export
batch_get_page_links <- function(
    pages_list,
    force_restart = FALSE,
    checkpoint_file = "links_checkpoint.rds",
    chunk_size = 50,
    checkpoint_interval = 10,
    delete_temp = TRUE
) {

  chunked_pages_list <- split(
    pages_list,
    ceiling(seq_along(pages_list) / chunk_size)
  )

  checkpoint <- load_checkpoint(
    checkpoint_file,
    force_restart,
    default_value = list(
      links_batch = vector(
        "list",
        length(chunked_pages_list)
      ),
      next_index = 1
    ),
    input_length = length(pages_list)
  )

  links_batch <- checkpoint$links_batch
  start_i <- checkpoint$next_index

  for (i in start_i:length(chunked_pages_list)) {

    chunk <- chunked_pages_list[[i]]

    if (length(chunk) == 0 || all(is.na(chunk))) {
      next
    }

    links_batch[[i]] <- get_links_for_pages(chunk)

    next_index <- checkpoint_manager(
      i = i,
      input_size = length(chunked_pages_list),
      checkpoint_interval = checkpoint_interval,
      checkpoint_file = checkpoint_file,
      state = list(
        links_batch = links_batch,
        input_length = length(chunked_pages_list)
      )
    )
  }

  cleanup_checkpoint(
    checkpoint_file,
    delete_temp = delete_temp
  )

  do.call(rbind, links_batch)
}
