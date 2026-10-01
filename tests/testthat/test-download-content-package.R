test_that("download_content_package creates page folders and writes content", {
  package_path <- tempfile("appropedia-package-")
  dir.create(package_path)
  withr::defer(unlink(package_path, recursive = TRUE))

  assets <- data.frame(
    page_title = "Example project",
    page_id = 10L,
    page_revision_id = 20L,
    file_title = "File:Example image.jpg",
    canonical_file_title = "File:Example image.jpg",
    file_url = "https://example.org/Example_image.jpg",
    asset_class = "image",
    sha1 = "abc123",
    status = "ok",
    error = NA_character_,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    .get_page_export_content = function(
    page,
    include_wikitext,
    include_html,
    handle = NULL
    ) {
      list(
        requested_title = page,
        page_title = "Example project",
        page_id = 10L,
        page_revision_id = 20L,
        wikitext = "== Example ==",
        html = "<div class=\"mw-parser-output\"><h2>Example</h2></div>",
        status = "ok",
        error = NA_character_
      )
    },
    .download_asset_file = function(url, destination, overwrite) {
      writeBin(charToRaw("image"), destination)
      list(
        status = "downloaded",
        http_status = 200L,
        bytes = unname(file.info(destination)$size),
        error = NA_character_
      )
    },
    .package = "appropedia"
  )

  report <- download_content_package(
    "Example project",
    path = package_path,
    assets = assets
  )

  page_path <- file.path(package_path, "Example project")

  expect_true(dir.exists(file.path(page_path, "wikitext")))
  expect_true(dir.exists(file.path(page_path, "html")))
  expect_true(dir.exists(file.path(page_path, "images")))
  expect_true(dir.exists(file.path(page_path, "manufacturing")))
  expect_true(dir.exists(file.path(page_path, "documents")))
  expect_true(dir.exists(file.path(page_path, "other")))

  expect_true(file.exists(file.path(
    page_path,
    "wikitext",
    "Example project.wiki"
  )))
  expect_true(file.exists(file.path(
    page_path,
    "html",
    "Example project.html"
  )))
  expect_true(file.exists(file.path(
    page_path,
    "images",
    "Example image.jpg"
  )))
  expect_true(file.exists(file.path(package_path, "download-report.csv")))
  expect_equal(nrow(report), 3L)
})

test_that("the same asset is downloaded into every relevant page", {
  package_path <- tempfile("appropedia-package-")
  dir.create(package_path)
  withr::defer(unlink(package_path, recursive = TRUE))

  assets <- data.frame(
    page_title = c("Page one", "Page two"),
    page_id = c(1L, 2L),
    page_revision_id = c(11L, 22L),
    file_title = rep("File:Shared.stl", 2L),
    canonical_file_title = rep("File:Shared.stl", 2L),
    file_url = rep("https://example.org/Shared.stl", 2L),
    asset_class = rep("manufacturing", 2L),
    sha1 = rep("same-sha1", 2L),
    status = rep("ok", 2L),
    error = rep(NA_character_, 2L),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    .get_page_export_content = function(
    page,
    include_wikitext,
    include_html,
    handle = NULL
    ) {
      page_id <- if (page == "Page one") 1L else 2L

      list(
        requested_title = page,
        page_title = page,
        page_id = page_id,
        page_revision_id = page_id * 11L,
        wikitext = "content",
        html = "<p>content</p>",
        status = "ok",
        error = NA_character_
      )
    },
    .download_asset_file = function(url, destination, overwrite) {
      writeBin(charToRaw("stl"), destination)
      list(
        status = "downloaded",
        http_status = 200L,
        bytes = unname(file.info(destination)$size),
        error = NA_character_
      )
    },
    .package = "appropedia"
  )

  report <- download_content_package(
    c("Page one", "Page two"),
    path = package_path,
    assets = assets,
    include_wikitext = FALSE,
    include_html = FALSE
  )

  expect_true(file.exists(file.path(
    package_path,
    "Page one",
    "manufacturing",
    "Shared.stl"
  )))
  expect_true(file.exists(file.path(
    package_path,
    "Page two",
    "manufacturing",
    "Shared.stl"
  )))
  expect_equal(sum(report$download_status == "downloaded"), 2L)
})

test_that("unresolved assets are reported without being downloaded", {
  package_path <- tempfile("appropedia-package-")
  dir.create(package_path)
  withr::defer(unlink(package_path, recursive = TRUE))

  assets <- data.frame(
    page_title = "Example",
    page_id = 1L,
    page_revision_id = 2L,
    file_title = "File:Missing.pdf",
    canonical_file_title = NA_character_,
    file_url = NA_character_,
    asset_class = "document",
    sha1 = NA_character_,
    status = "unresolved",
    error = "File is unavailable.",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    .get_page_export_content = function(
    page,
    include_wikitext,
    include_html,
    handle = NULL
    ) {
      list(
        requested_title = page,
        page_title = page,
        page_id = 1L,
        page_revision_id = 2L,
        wikitext = "content",
        html = "<p>content</p>",
        status = "ok",
        error = NA_character_
      )
    },
    .download_asset_file = function(url, destination, overwrite) {
      stop("This mock should not be called.")
    },
    .package = "appropedia"
  )

  report <- download_content_package(
    "Example",
    path = package_path,
    assets = assets,
    include_wikitext = FALSE,
    include_html = FALSE
  )

  expect_equal(report$download_status, "unresolved")
  expect_equal(report$error, "File is unavailable.")
  expect_false(file.exists(file.path(
    package_path,
    "Example",
    "documents",
    "Missing.pdf"
  )))
})

test_that("existing files are not overwritten by default", {
  package_path <- tempfile("appropedia-package-")
  destination <- file.path(
    package_path,
    "Example",
    "images",
    "Existing.png"
  )
  dir.create(dirname(destination), recursive = TRUE)
  writeBin(charToRaw("original"), destination)
  withr::defer(unlink(package_path, recursive = TRUE))

  result <- .download_asset_file(
    "https://example.org/Existing.png",
    destination,
    overwrite = FALSE
  )

  expect_equal(result$status, "skipped_existing")
  expect_equal(rawToChar(readBin(destination, "raw", n = 100L)), "original")
})

test_that("partial asset discovery does not prevent a valid download", {
  package_path <- tempfile("appropedia-package-")
  dir.create(package_path)
  withr::defer(unlink(package_path, recursive = TRUE))

  assets <- data.frame(
    page_title = "Example",
    page_id = 1L,
    page_revision_id = 2L,
    file_title = "File:Part.stl",
    canonical_file_title = "File:Part.stl",
    file_url = "https://example.org/Part.stl",
    asset_class = "manufacturing",
    sha1 = "abc123",
    status = "partial",
    error = "Rendered asset discovery was incomplete.",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    .get_page_export_content = function(
    page,
    include_wikitext,
    include_html,
    handle = NULL
    ) {
      list(
        requested_title = page,
        page_title = page,
        page_id = 1L,
        page_revision_id = 2L,
        wikitext = NA_character_,
        html = NA_character_,
        status = "ok",
        error = NA_character_
      )
    },
    .download_asset_file = function(url, destination, overwrite) {
      writeBin(charToRaw("part"), destination)
      list(
        status = "downloaded",
        http_status = 200L,
        bytes = unname(file.info(destination)$size),
        error = NA_character_
      )
    },
    .package = "appropedia"
  )

  report <- download_content_package(
    "Example",
    path = package_path,
    assets = assets,
    include_wikitext = FALSE,
    include_html = FALSE
  )

  expect_equal(report$source_status, "partial")
  expect_equal(report$download_status, "downloaded")
  expect_true(file.exists(file.path(
    package_path,
    "Example",
    "manufacturing",
    "Part.stl"
  )))
})
