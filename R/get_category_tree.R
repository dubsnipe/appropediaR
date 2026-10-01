#' Build Appropedia's category tree as a dictionary
#'
#' @return A database that pairs parent and child categories.
#'
#' @export
get_category_tree <- function() {

  categories <- get_all_categories(verbose = FALSE)

  categories_with_children <- categories$category[
    categories$subcats > 0
  ]

  tree <- lapply(
    seq_along(categories_with_children),
    function(i) {

      cat(
        i, "/",
        length(categories_with_children),
        ":",
        categories_with_children[i],
        "\n"
      )

      get_category_subcategories(
        categories_with_children[i]
      )
    }
  )

  do.call(rbind, tree)
}
