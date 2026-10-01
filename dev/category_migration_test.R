require(googlesheets4)

google_s <- "https://docs.google.com/spreadsheets/d/1pHTX0UnUxqMDG8hcUdvme4LD2TndYGqVzYfJXb37Uss/edit?usp=sharing"

cat_all <- read_sheet(ss = google_s, sheet = 1)

parameter_map <- data.frame(
  facet = c(
    "process", "resource", "appr_tech",
    "affiliation", "location", "environment-space"
  ),
  template_name = c(
    "Project data", "Project data", "Project data",
    "Page data", "Project data", "Project data"
  ),
  param_name = c(
    "uses", "material", "type",
    "organizations", "location", "environment"
  ),
  separator = "; ",
  stringsAsFactors = FALSE
)

test_category_migration <- function(
    category,
    review_data = cat_all,
    max_pages = 5,
    merge = "append"
) {
  pages <- get_pages_from_category(
    category,
    verbose = FALSE
  )

  pages <- head(pages, max_pages)

  api_df <- get_categories_for_pages(pages)

  api_by_page <- split(
    api_df$category,
    api_df$page
  )

  results <- lapply(
    pages,
    function(page) {
      content <- get_page_content(page)

      if (is.null(content)) {
        return(list(
          page = page,
          status = "page_not_found"
        ))
      }

      api_categories <- api_by_page[[page]]
      api_categories <- api_categories[
        !is.na(api_categories)
      ]

      result <- transform_page_categories(
        content = content,
        category_plan = review_data,
        parameter_map = parameter_map,
        api_categories = api_categories,
        merge = merge
      )

      c(list(page = page), result)
    }
  )

  names(results) <- pages
  results
}

test <- test_category_migration(
  category = "Water",
  max_pages = 10
)

test[[1]]$status
test[[1]]$warnings
test[[1]]$parameter_changes
cat(test[[1]]$template_snippet_before)
cat(test[[1]]$template_snippet_after)
cat(test[[1]]$category_block_before)
cat(test[[1]]$category_block_after)
