#' sort_redirects.R


#' Get the immediate redirect target for a page
#'
#' Retrieves the direct redirect target of a page using the MediaWiki API.
#'
#' This helper function is used by \code{\link{resolve_redirect}} to follow
#' redirect chains until a final destination page is reached.
#'
#' @param page_name Character string containing the page title.
#' @param api_url Character string containing the MediaWiki API endpoint.
#'
#' @return Character string containing the redirect target if the page is a
#' redirect, or \code{NULL} if the page is not a redirect.
#'
#' @seealso \code{\link{resolve_redirect}}
#'
#' @examples
#' \dontrun{
#' get_redirect_url("Greywater")
#' }
get_redirect_url <- function(page_name,
                             api_url = get_appropedia_api_url()) {
  tryCatch({
    res <- httr::GET(api_url, query = list(
      action = "query",
      prop = "info",
      redirects="",
      titles = page_name,
      format = "json"
    ), httr::timeout(5))

    pages <- httr::content(res, as = "parsed",
                     type = "application/json"
    )

    if (!is.null(pages$query$redirects[[1]]$to)) {
      response <-
        stringr::str_replace_all(pages$query$redirects[[1]]$to, " ", "_")
      return(response)
    } else {
      return(NULL)  # Explicitly return NULL for missing wikitext
    }
  })
}


#' Resolve the final target of a redirect chain
#'
#' Starting from a page title, follows redirects until a non-redirect page
#' is reached.
#'
#' This function is useful when working with page lists that may contain
#' outdated titles or redirects.
#'
#' Redirect loops are detected automatically and processing stops if a cycle
#' is encountered.
#'
#' @param page_name Character string containing the page title.
#' @param seen Internal parameter used for redirect loop detection.
#'
#' @return Character string containing the final page title.
#'
#' @seealso
#' \code{\link{get_redirect_url}},
#' \code{\link{apply_redirects}}
#'
#' @examples
#' \dontrun{
#' resolve_redirect("Greywater")
#' }
#'
#' @export
resolve_redirect <- function(page_name, seen = character()) {

  # Prevent infinite loops if there's a redirect cycle
  if (page_name %in% seen) {
    return(page_name)
  }

  redirect_url <- get_redirect_url(page_name)

  if (!is.null(redirect_url) && nchar(redirect_url) > 0) {
    # Follow the redirect recursively
    return(resolve_redirect(redirect_url, c(seen, page_name)))
  } else {
    # No redirect, return final destination
    return(page_name)
  }
}


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
