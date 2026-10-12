
#' Get the checkpoint directory
#'
#' Returns the directory used by the package to store checkpoint files for
#' long-running workflows.
#'
#' The directory is created automatically if it does not already exist.
#'
#' @return Character string containing the checkpoint directory path.
#'
#' @examples
#' \dontrun{
#' get_checkpoint_dir()
#'}
get_checkpoint_dir <- function() {

  dir <- tools::R_user_dir(
    "appropedia",
    which = "data"
  )

  dir.create(
    dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir
}
