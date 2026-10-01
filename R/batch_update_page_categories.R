#' Batch update categories on multiple pages.
#'
#' Applies a set of category modifications across a collection of pages.
#' Progress can be checkpointed and resumed if interrupted.
#'
#' @param pages_list Character vector containing page titles.
#' @param changes Data frame containing category modifications.
#' @param session MediaWiki session object.
#' @param dry_run Logical indicating whether edits should be saved.
#' @param force_restart Logical indicating whether an existing checkpoint
#' should be ignored.
#' @param checkpoint_file Character string containing checkpoint filename.
#' @param checkpoint_interval Number of pages processed between checkpoint
#' saves.
#' @param delete_temp Logical indicating whether the checkpoint file should
#' be removed after successful completion.
#'
#' @return Data frame containing one row per page and edit outcome.
#'
#' @export
batch_update_page_categories <- function(
    pages_list,
    changes,
    session,
    dry_run = TRUE,
    force_restart = FALSE,
    checkpoint_file = "category_updates_checkpoint.rds",
    checkpoint_interval = 10,
    delete_temp = TRUE
) {

  run_id <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

  checkpoint <- load_checkpoint(
    checkpoint_file = checkpoint_file,
    force_restart = force_restart,
    default_value = list(
      reports = vector("list", length(pages_list)),
      next_index = 1,
      run_id = run_id
    ),
    input_length = length(pages_list)
  )

  reports <- checkpoint$reports
  start_i <- checkpoint$next_index

  for (i in start_i:length(pages_list)) {

    page_name <- pages_list[i]

    cat("Processing", i, "of", length(pages_list), ":", page_name, "\n")

    reports[[i]] <- update_page_categories(
      page_name = page_name,
      changes = changes,
      session = session,
      dry_run = dry_run
    )

    reports[[i]]$run_id <- run_id

    checkpoint_manager(
      i = i,
      input_size = length(pages_list),
      checkpoint_interval = checkpoint_interval,
      checkpoint_file = checkpoint_file,
      state = list(
        reports = reports,
        next_index = i + 1,
        run_id = run_id,
        input_length = length(pages_list)
      )
    )
  }

  if (delete_temp) {
    cleanup_checkpoint(checkpoint_file)
  }

  dplyr::bind_rows(lapply(reports, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
}
