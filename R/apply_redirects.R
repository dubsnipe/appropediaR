#' Resolve redirects for a list of pages
#'
#' Resolves redirects for an entire vector of page titles.
#'
#' This function is useful for cleaning page lists before performing metadata
#' extraction, category analysis, or bulk editing operations.
#'
#' Progress is periodically saved to a checkpoint file so that interrupted
#' workflows can be resumed without restarting from the beginning.
#'
#' This function preserves the original vector order.
#'
#' @param names_list Character vector containing page titles.
#' @param force_restart Logical indicating whether an existing checkpoint
#' should be deleted and processing restarted from the beginning.
#' @param checkpoint_file Character string containing the checkpoint filename
#' or file path.
#' @param checkpoint_interval Number of pages processed between checkpoint
#' saves.
#'
#' @return Character vector containing the resolved page titles.
#'
#' @seealso
#' \code{\link{resolve_redirect}},
#' \code{\link{load_checkpoint}},
#' \code{\link{checkpoint_manager}}
#'
#' @examples
#' \dontrun{
#' pages <- c(
#'   "Greywater",
#'   "Composting toilet"
#' )
#'
#' resolved_pages <- apply_redirects(pages)
#' }
#'
#' @export
apply_redirects <- function(names_list,
                            force_restart = FALSE,
                            checkpoint_file = "redirects_checkpoint.rds",
                            checkpoint_interval = 100) {

  checkpoint <- load_checkpoint(
    checkpoint_file,
    force_restart,
    default_value = list(
      resolved = character(length(names_list)),
      next_index = 1
    ),
    input_length = length(names_list)
  )

  resolved <- checkpoint$resolved
  start_i <- checkpoint$next_index

  for (i in start_i:length(names_list)) {
    cat("Resolving redirects for row", i, ":", names_list[i], "\n")

    # Handle empty or NA entries
    if (is.na(names_list[i]) || names_list[i] == "") {
      cat(" \u2192 Empty row, skipping\n")
      resolved[i] <- names_list[i]
      next
    }

    final_target <- resolve_redirect(names_list[i])
    if (final_target != names_list[i]) {
      cat(" \u2192 Redirect resolved to:", final_target, "\n")
      resolved[i] <- final_target
    } else {
      cat(" \u2192 No redirect\n")
      resolved[i] <- names_list[i]
    }

    next_index <- checkpoint_manager(
      i = i,
      input_size = length(names_list),
      checkpoint_interval = checkpoint_interval,
      checkpoint_file = checkpoint_file,
      state = list(
        resolved = resolved,
        input_length = length(names_list)
        )
    )
  }

  cleanup_checkpoint(checkpoint_file)
  message("Redirect resolution complete. Total rows processed: ", length(names_list), "\n")
  # cat("Original pages list size:", length(names_list), "\n")
  return(resolved)
}
