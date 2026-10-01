#' Retrieve subcategories for a category
#'
#' @param category Category name without the "Category:" prefix.
#'
#' @return A data frame containing parent-child category relationships.
#'
#' @export
get_category_subcategories <- function(category) {

  subcategories <- get_pages_from_category(
    category = category,
    namespace = 14
  )

  if (length(subcategories) == 0) {
    return(
      data.frame(
        parent_category = character(),
        child_category = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  data.frame(
    parent_category = category,
    child_category = sub("^Category:", "", subcategories),
    stringsAsFactors = FALSE
  )
}
