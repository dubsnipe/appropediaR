#' helpers.R


#' Validate an authenticated session
#'
#' Verifies that a \code{wiki_session} object is valid and can be used for
#' authenticated API requests.
#'
#' This function is intended for use at the start of workflows that require an
#' authenticated session. If the session is invalid, the workflow is stopped
#' and the user is instructed to authenticate again using
#' \code{\link{do_login}}.
#'
#' @param session A \code{wiki_session} object returned by
#'   \code{\link{do_login}}.
#'
#' @return Invisibly returns \code{TRUE} if the session is valid.
#' An error is raised if the session is invalid.
#'
#' @seealso \code{\link{do_login}}
#'
#' @examples
#' \dontrun{
#' session <- do_login()
#' stop_if_invalid_session(session)
#' }
stop_if_invalid_session <- function(session) {
  
  if (missing(session) || is.null(session)) {
    stop("A valid wiki_session is required.", call. = FALSE)
  }
  
  if (!inherits(session, "wiki_session")) {
    stop(
      "session must be a wiki_session object returned by do_login().",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


#' Execute a MediaWiki API query
#'
#' Performs a MediaWiki API request and returns the parsed JSON response.
#'
#' This helper function centralizes API communication for read operations that
#' do not require a CSRF token.
#'
#' @param query Named list containing MediaWiki API query parameters.
#' @param handle Optional \code{httr} handle used to maintain session state
#' across requests.
#'
#' @return A parsed JSON object represented as a nested R list.
#'
#' @seealso \code{\link{appropedia_save}}
#'
#' @examples
#' \dontrun{
#' appropedia_query(
#'   list(
#'     action = "query",
#'     meta = "siteinfo",
#'     format = "json"
#'   )
#' )
#' }
appropedia_query <- function(
    query,
    handle = NULL
) {

  response <- httr::GET(
    get_appropedia_api_url(),
    query = query,
    handle = handle
  )

  httr::stop_for_status(response)

  jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8")
  )

}


#' Save a page through the MediaWiki API
#'
#' Performs an authenticated page edit using the MediaWiki API.
#'
#' This helper function centralizes API communication for operations that
#' require a CSRF token, such as page edits.
#'
#' @param page_name Character string containing the page title.
#' @param content Character string containing the new page content.
#' @param session A \code{wiki_session} object returned by
#'   \code{\link{do_login}}.
#' @param summary Edit summary recorded in the page history.
#' @param bot Logical indicating whether the edit should be marked as a bot
#' edit.
#'
#' @return The parsed API response.
#'
#' @seealso \code{\link{do_login}},
#'   \code{\link{appropedia_query}}
#'
#' @examples
#' \dontrun{
#' appropedia_save(
#'   page_name = "Sandbox",
#'   content = "Example edit",
#'   session = session
#' )
#' }
appropedia_save <- function(
    page_name,
    content,
    session,
    summary = "Automatic maintenance edit",
    bot = TRUE
) {
  
  stop_if_invalid_session(session)
  
  res <- retry_request(httr::POST(
    get_appropedia_api_url(),
    body = list(
      action = "edit",
      title = page_name,
      text = content,
      summary = summary,
      token = session$csrf_token,
      format = "json",
      bot = as.integer(bot)
    ),
    encode = "form",
    handle = session$handle
  ))
  
  response <- httr::content(
    res,
    as = "parsed",
    type = "application/json"
  )
  
  if (!is.null(response$edit) &&
      response$edit$result == "Success") {
    return(TRUE)
  }
  
  print(response)
  
  FALSE
}


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
#' get_checkpoint_dir()
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


#' Resolve a checkpoint file path
#'
#' Resolves the full path used for a checkpoint file.
#'
#' If a filename is supplied without a directory component, the file is stored
#' in the package checkpoint directory returned by
#' \code{\link{get_checkpoint_dir}}.
#'
#' If a complete path is supplied, it is used unchanged.
#'
#' @param checkpoint_file Character string containing a checkpoint filename or
#' file path.
#'
#' @return Character string containing the resolved file path.
#'
#' @seealso \code{\link{get_checkpoint_dir}}
#'
#' @examples
#' resolve_checkpoint_path("checkpoint.rds")
resolve_checkpoint_path <- function(checkpoint_file) {
  
  if (is.null(checkpoint_file)) {
    return(
      file.path(
        get_checkpoint_dir(),
        "checkpoint.rds"
      )
    )
  }
  
  # no path supplied, only filename
  if (basename(checkpoint_file) == checkpoint_file) {
    return(
      file.path(
        get_checkpoint_dir(),
        checkpoint_file
      )
    )
  }
  
  # user supplied full or relative path
  checkpoint_file
}


#' Load a checkpoint file
#'
#' Loads a checkpoint file for a long-running workflow.
#'
#' If the checkpoint file exists, its contents are returned. If
#' \code{force_restart = TRUE}, the existing checkpoint is removed and the
#' supplied default value is returned instead.
#'
#' @param checkpoint_file Character string containing the checkpoint filename
#' or file path.
#' @param force_restart Logical indicating whether an existing checkpoint
#' should be deleted and the workflow restarted.
#' @param default_value Object returned when no checkpoint exists or when
#' restarting.
#'
#' @return The checkpoint object or \code{default_value}.
#'
#' @seealso
#' \code{\link{cleanup_checkpoint}},
#' \code{\link{validate_checkpoint_structure}},
#' \code{\link{validate_checkpoint_length}}
#'
#' @examples
#' \dontrun{
#' checkpoint <- load_checkpoint(
#'   "redirects_checkpoint.rds",
#'   default_value = list(
#'     resolved = character(),
#'     next_index = 1
#'   )
#' )
#' }
load_checkpoint <- function(checkpoint_file, 
                            force_restart = FALSE, 
                            default_value = NULL) {
  
  resolved_checkpoint_file <- resolve_checkpoint_path(checkpoint_file)
  
  if (force_restart && file.exists(resolved_checkpoint_file)) {
    file.remove(resolved_checkpoint_file)
    message("Existing checkpoint removed. Starting from scratch.")
  }
  
  if (file.exists(resolved_checkpoint_file)) {
    return(readRDS(resolved_checkpoint_file))
  }
  
  
  default_value
}


#' Validate checkpoint structure
#'
#' Verifies that a checkpoint object contains the required fields expected by
#' the current workflow.
#'
#' This helper is intended to detect incompatible or corrupted checkpoint
#' files before processing resumes.
#'
#' @param checkpoint Checkpoint object loaded from disk.
#' @param required_fields Character vector containing required field names.
#'
#' @return Invisibly returns \code{TRUE} if validation succeeds.
#' An error is raised if validation fails.
#'
#' @examples
#' \dontrun{
#' validate_checkpoint_structure(
#'   checkpoint,
#'   c("resolved", "next_index")
#' )
#' }
validate_checkpoint_structure <- function(
    checkpoint,
    required_fields
) {
  
  missing <- setdiff(
    required_fields,
    names(checkpoint)
  )
  
  if (length(missing) > 0) {
    stop(
      paste(
        "Checkpoint is incompatible with the current function version.",
        "Missing field(s):",
        paste(missing, collapse = ", "),
        "Use force_restart = TRUE or delete:",
        checkpoint_file
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


#' Validate checkpoint length
#'
#' Verifies that a checkpoint is compatible with the current input data.
#'
#' This helper is intended to detect situations where a workflow is resumed
#' using a different input vector than the one used to create the checkpoint.
#'
#' @param checkpoint Checkpoint object loaded from disk.
#' @param current_length Length of the current input data.
#' @param checkpoint_file Character string containing the checkpoint filename
#' or file path.
#'
#' @return Invisibly returns \code{TRUE} if validation succeeds.
#' An error is raised if validation fails.
#'
#' @examples
#' \dontrun{
#' validate_checkpoint_length(
#'   checkpoint,
#'   current_length = length(names_list),
#'   checkpoint_file = "redirects_checkpoint.rds"
#' )
#' }
validate_checkpoint_length <- function(
    checkpoint,
    current_length,
    checkpoint_file
) {
  
  actual_length <- checkpoint$input_length
  
  # Fresh run or legacy checkpoint
  if (is.null(actual_length)) {
    return(invisible(TRUE))
  }
  
  # Malformed checkpoint
  if (
    !is.numeric(actual_length) ||
    length(actual_length) != 1
  ) {
    
    warning(
      paste(
        "Checkpoint",
        checkpoint_file,
        "contains invalid input_length. Ignoring validation."
      )
    )
    
    return(invisible(TRUE))
  }
  
  if (actual_length != current_length) {
    
    stop(
      paste(
        "Checkpoint was created for",
        actual_length,
        "items but current input contains",
        current_length,
        "items.",
        "Use force_restart = TRUE or delete:",
        checkpoint_file
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}
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
#' @param n Total number of iterations.
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
#'   n = length(names_list),
#'   checkpoint_interval = 100,
#'   checkpoint_file = "redirects_checkpoint.rds",
#'   state = list(
#'     resolved = resolved
#'   )
#' )
#' }
checkpoint_manager <- function(
    i,
    n,
    checkpoint_interval,
    checkpoint_file,
    state,
    label = "Checkpoint"
) {
  
  checkpoint_file <- resolve_checkpoint_path(checkpoint_file)
  next_index <- i + 1
  
  if (i %% checkpoint_interval == 0 && i < n) {
    next_index <- if (i < n) i + 1 else i
    
    saveRDS(
      c(state, list(next_index = next_index, input_length = n)),
      checkpoint_file
      )
    
    cat(label, "saved at row", i, "\n")
  }
  
  next_index
}
# Future recommendation: split checkpoint system into:
#   
# checkpoint_vector_state()   # redirects, linear processing
# checkpoint_cursor_state()   # SMW, pagination


#' Delete a checkpoint file
#'
#' Removes a checkpoint file after a workflow has completed successfully.
#'
#' This helper is typically called at the end of an iterative process to avoid
#' resuming from stale state in future runs.
#'
#' @param checkpoint_file Character string containing the checkpoint filename
#' or file path.
#'
#' @return Invisibly returns \code{TRUE} if the file was removed or does not
#' exist.
#'
#' @examples
#' \dontrun{
#' cleanup_checkpoint("redirects_checkpoint.rds")
#' }
cleanup_checkpoint <- function(checkpoint_file) {
  resolved_checkpoint_file <- resolve_checkpoint_path(checkpoint_file)
  if (file.exists(resolved_checkpoint_file)) {
    file.remove(resolved_checkpoint_file)
    cat("Task done. Checkpoint removed.\n")
  }
  invisible(TRUE)
}


#' Retry a potentially failing operation
#'
#' Executes an expression and automatically retries when an error occurs.
#'
#' This helper is intended for network requests and other transient operations
#' that may fail temporarily.
#'
#' Delays between retries increase by an exponential backoff strategy until
#' until reaching the specified maximum delay.
#'
#' @param expr Expression to evaluate.
#' @param max_retries Maximum number of retry attempts.
#' @param initial_delay Initial delay in seconds before retrying.
#' @param max_delay Maximum delay in seconds between retry attempts.
#'
#' @return The result of the evaluated expression.
#'
#' @examples
#' \dontrun{
#' retry_request(
#'   appropedia_query(
#'     list(
#'       action = "query",
#'       meta = "siteinfo",
#'       format = "json"
#'     )
#'   )
#' )
#' }
retry_request <- function(
    expr,
    max_retries = 5,
    initial_delay = 5,
    max_delay = 60
) {
  
  delay <- initial_delay
  
  for (attempt in seq_len(max_retries)) {
    
    result <- tryCatch(
      eval.parent(substitute(expr)),
      error = function(e) e
    )
    
    if (!inherits(result, "error")) {
      return(result)
    }
    
    message(
      "Attempt ",
      attempt,
      " failed: ",
      conditionMessage(result)
    )
    
    if (attempt < max_retries) {
      Sys.sleep(delay)
      delay <- min(delay * 2, max_delay)
    }
  }
  
  stop("Maximum retries exceeded.")
}

