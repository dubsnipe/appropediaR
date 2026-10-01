# Note: consider hashes for a better future validator.


#' Manage checkpoint creation during iteration
#'
#' Creates or updates a checkpoint file during an iterative workflow.
#'
#' This helper centralizes checkpoint logic for long-running processes such as
#' redirect resolution, category extraction, and semantic metadata collection.
#'
#' A checkpoint is written when the current iteration reaches a checkpoint
#' interval or the final iteration.
#'
#' In addition to the user-supplied state, the checkpoint automatically stores
#' the next iteration index and the total input length required to resume and
#' validate the workflow.
#'
#' @param i Current iteration index.
#' @param input_size Total number of iterations.
#' @param checkpoint_interval Number of iterations between checkpoint saves.
#' @param checkpoint_file Character string containing the checkpoint filename
#' or file path.
#' @param state Named list containing the workflow state to save.
#' Do not include \code{next_index} or \code{input_length}; these are added
#' automatically.
#' @param label Character string used when printing progress messages.
#'
#' @return Integer indicating the next iteration index that should be processed
#' if the workflow is resumed from the checkpoint.
#'
#' @details
#' The checkpoint file contains:
#' \itemize{
#'   \item User-supplied workflow state.
#'   \item \code{next_index}, the next iteration to process.
#'   \item \code{input_length}, the total number of items in the original input.
#' }
#'
#' These metadata fields are used by
#' \code{\link{load_checkpoint}},
#' \code{\link{validate_checkpoint_structure}}, and
#' \code{\link{validate_checkpoint_length}} to safely resume interrupted
#' workflows.
#'
#' @seealso
#' \code{\link{load_checkpoint}},
#' \code{\link{cleanup_checkpoint}},
#' \code{\link{validate_checkpoint_structure}},
#' \code{\link{validate_checkpoint_length}}
#'
#' @examples
#' \dontrun{
#' next_index <- checkpoint_manager(
#'   i = i,
#'   input_size = length(names_list),
#'   checkpoint_interval = 100,
#'   checkpoint_file = "redirects_checkpoint.rds",
#'   state = list(
#'     resolved = resolved
#'   )
#' )
#' }
#' @export
checkpoint_manager <- function(
    i,
    input_size,
    checkpoint_interval,
    checkpoint_file,
    state,
    label = "Checkpoint"
) {

  checkpoint_file <- resolve_checkpoint_path(checkpoint_file)
  next_index <- i + 1

  if (i %% checkpoint_interval == 0 && i < input_size) {
    next_index <- if (i < input_size) i + 1 else i

    saveRDS(
      c(state, list(next_index = next_index, input_length = input_size)),
      checkpoint_file
      )

    cat(label, "saved at row", i, "\n")
  }

  next_index
}
