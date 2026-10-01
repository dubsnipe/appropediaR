test_that("category cleanup and parameter migration are composed", {
  content <- paste(
    "{{Page data",
    "| organizations = Existing org",
    "}}",
    "{{Project data",
    "| uses = Water treatment",
    "}}",
    "Text",
    "[[Category:Old name]]",
    "[[Category:Solar cooking, Water heating]]",
    "[[Category:Clay]]",
    "[[Category:Obsolete]]",
    sep = "\n"
  )

  plan <- data.frame(
    category = c(
      "Old name", "Solar cooking, Water heating", "Clay", "Obsolete"
    ),
    process = c(FALSE, TRUE, FALSE, FALSE),
    material = c(FALSE, FALSE, TRUE, FALSE),
    delete = c(FALSE, FALSE, FALSE, TRUE),
    rename = c("New name", NA, NA, NA),
    `add or split` = c(NA, "Solar cooking|Water heating", NA, NA),
    check.names = FALSE
  )

  map <- data.frame(
    facet = c("process", "material"),
    template_name = c("Project data", "Project data"),
    param_name = c("uses", "material"),
    separator = c("; ", "; ")
  )

  result <- transform_page_categories(content, plan, map)

  expect_equal(result$status, "dry_run_changed")
  expect_equal(result$explicit_after, "New name")
  expect_match(result$content, "uses = Water treatment; Solar cooking; Water heating")
  expect_match(result$content, "material = Clay")
  expect_match(result$category_block_after, "\\[\\[Category:New name\\]\\]")
  expect_false(grepl("Obsolete", result$content, fixed = TRUE))
})

test_that("implicit categories are reported but not edited", {
  content <- "Text\n[[Category:Explicit]]\n"
  plan <- data.frame(
    category = "Implicit",
    delete = TRUE
  )
  map <- data.frame(
    facet = character(),
    template_name = character(),
    param_name = character()
  )

  result <- transform_page_categories(
    content,
    plan,
    map,
    api_categories = c("Explicit", "Implicit")
  )

  expect_equal(result$implicit_categories, "Implicit")
  expect_equal(result$status, "no_change")
})

test_that("multiple mapped facets produce a conflict", {
  content <- "{{Project data}}\n[[Category:Example]]\n"
  plan <- data.frame(
    category = "Example",
    process = TRUE,
    material = TRUE
  )
  map <- data.frame(
    facet = c("process", "material"),
    template_name = c("Project data", "Project data"),
    param_name = c("uses", "material")
  )

  result <- transform_page_categories(content, plan, map)

  expect_equal(result$status, "conflict")
  expect_length(result$conflicts, 1)
  expect_equal(result$content, content)
})

test_that("risky markup is flagged", {
  content <- paste(
    "<!-- [[Category:Example category]] -->",
    "<nowiki>[[Category:Another example]]</nowiki>",
    "{{Project data|uses={{Nested template}}}}",
    "[[Category:Real category]]",
    sep = "\n"
  )
  plan <- data.frame(category = "Real category", delete = TRUE)
  map <- data.frame(
    facet = character(),
    template_name = character(),
    param_name = character()
  )

  result <- transform_page_categories(content, plan, map)

  expect_true(all(c("nowiki", "html_comment", "nested_template") %in% result$warnings))
})

test_that("existing parameter values can be skipped", {
  content <- paste(
    "{{Project data",
    "| uses = Existing value",
    "}}",
    "[[Category:New use]]",
    sep = "\n"
  )
  plan <- data.frame(category = "New use", process = TRUE)
  map <- data.frame(
    facet = "process",
    template_name = "Project data",
    param_name = "uses"
  )

  result <- transform_page_categories(content, plan, map, merge = "skip")

  expect_equal(result$parameter_changes$action, "skipped_existing")
  expect_match(result$content, "uses = Existing value")
  expect_true("New use" %in% result$explicit_after)
})
