.collect_page_asset_relationships <- function(
    page,
    embedded,
    linked,
    transcluded,
    handle = NULL
) {
  context <- tryCatch(
    .get_page_asset_context(page, handle = handle),
    error = function(error) {
      list(
        page_title = page,
        page_id = NA_integer_,
        page_revision_id = NA_integer_,
        page_revision_timestamp = NA_character_,
        wikitext = NA_character_,
        status = "page_error",
        error = conditionMessage(error)
      )
    }
  )

  if (!identical(context$status, "ok")) {
    return(.page_error_row(context))
  }

  source_assets <- .extract_source_assets(context$wikitext)

  rendered_assets <- tryCatch(
    .get_rendered_asset_titles(context$page_title, handle = handle),
    error = function(error) {
      list(
        titles = character(),
        status = "partial",
        error = conditionMessage(error)
      )
    }
  )

  embedded_titles <- unique(source_assets$embedded)
  linked_titles <- unique(source_assets$linked)
  rendered_titles <- unique(rendered_assets$titles)

  embedded_keys <- .file_key(embedded_titles)
  linked_keys <- .file_key(linked_titles)
  rendered_keys <- .file_key(rendered_titles)

  transcluded_keys <- setdiff(
    rendered_keys,
    union(embedded_keys, linked_keys)
  )

  included_keys <- character()

  if (embedded) {
    included_keys <- union(included_keys, embedded_keys)
  }

  if (linked) {
    included_keys <- union(included_keys, linked_keys)
  }

  if (transcluded) {
    included_keys <- union(included_keys, transcluded_keys)
  }

  if (length(included_keys) == 0L) {
    return(.empty_page_assets())
  }

  title_candidates <- c(
    rendered_titles,
    embedded_titles,
    linked_titles
  )

  title_keys <- .file_key(title_candidates)
  keep <- !duplicated(title_keys)
  title_lookup <- stats::setNames(title_candidates[keep], title_keys[keep])

  file_titles <- unname(title_lookup[included_keys])

  data.frame(
    page_title = rep(context$page_title, length(included_keys)),
    page_id = rep(context$page_id, length(included_keys)),
    page_revision_id = rep(
      context$page_revision_id,
      length(included_keys)
    ),
    page_revision_timestamp = rep(
      context$page_revision_timestamp,
      length(included_keys)
    ),
    file_title = file_titles,
    canonical_file_title = NA_character_,
    file_url = NA_character_,
    description_url = NA_character_,
    repository = NA_character_,
    mime_type = NA_character_,
    extension = NA_character_,
    size = NA_real_,
    sha1 = NA_character_,
    file_timestamp = NA_character_,
    asset_class = NA_character_,
    is_embedded = included_keys %in% embedded_keys,
    is_linked = included_keys %in% linked_keys,
    is_transcluded = included_keys %in% transcluded_keys,
    status = rep(rendered_assets$status, length(included_keys)),
    error = rep(rendered_assets$error, length(included_keys)),
    stringsAsFactors = FALSE
  )
}

.get_page_asset_context <- function(page, handle = NULL) {
  response <- appropedia_query(
    list(
      action = "query",
      titles = page,
      prop = "revisions",
      rvprop = "ids|timestamp|content",
      rvslots = "main",
      redirects = 1,
      format = "json",
      formatversion = 2
    ),
    handle = handle
  )

  api_page <- response$query$pages[[1L]]

  if (isTRUE(api_page$missing)) {
    stop(sprintf("Page not found: %s", page), call. = FALSE)
  }

  revision <- api_page$revisions[[1L]]
  slot <- revision$slots$main

  wikitext <- .first_non_null(
    slot$content,
    slot[["*"]],
    revision$content,
    revision[["*"]]
  )

  if (is.null(wikitext)) {
    stop(
      sprintf("No wikitext was returned for page: %s", api_page$title),
      call. = FALSE
    )
  }

  list(
    page_title = api_page$title,
    page_id = api_page$pageid,
    page_revision_id = revision$revid,
    page_revision_timestamp = revision$timestamp,
    wikitext = wikitext,
    status = "ok",
    error = NA_character_
  )
}

.get_rendered_asset_titles <- function(page, handle = NULL) {
  response <- appropedia_query(
    list(
      action = "parse",
      page = page,
      prop = "images|links",
      redirects = 1,
      format = "json",
      formatversion = 2
    ),
    handle = handle
  )

  images <- response$parse$images

  if (is.null(images)) {
    images <- character()
  }

  images <- unlist(images, use.names = FALSE)
  images <- .as_file_title(images)

  links <- response$parse$links

  if (is.null(links)) {
    links <- list()
  }

  file_links <- vapply(
    links,
    function(link) {
      namespace <- .first_non_null(link$ns, NA_integer_)

      if (!namespace %in% c(-2L, 6L)) {
        return(NA_character_)
      }

      .as_file_title(link$title)
    },
    character(1)
  )

  titles <- unique(c(images, file_links[!is.na(file_links)]))

  list(
    titles = titles,
    status = "ok",
    error = NA_character_
  )
}

.extract_source_assets <- function(wikitext) {
  if (length(wikitext) != 1L || is.na(wikitext) || !nzchar(wikitext)) {
    return(list(embedded = character(), linked = character()))
  }

  embedded_matches <- .extract_regex_matches(
    wikitext,
    "\\[\\[\\s*(?:File|Image)\\s*:\\s*[^\\]|#\\n]+"
  )

  embedded <- sub(
    "^\\[\\[\\s*(?:File|Image)\\s*:\\s*",
    "",
    embedded_matches,
    perl = TRUE,
    ignore.case = TRUE
  )

  linked_file_matches <- .extract_regex_matches(
    wikitext,
    "\\[\\[\\s*:\\s*(?:File|Image)\\s*:\\s*[^\\]|#\\n]+"
  )

  linked_files <- sub(
    "^\\[\\[\\s*:\\s*(?:File|Image)\\s*:\\s*",
    "",
    linked_file_matches,
    perl = TRUE,
    ignore.case = TRUE
  )

  media_matches <- .extract_regex_matches(
    wikitext,
    "\\[\\[\\s*Media\\s*:\\s*[^\\]|#\\n]+"
  )

  media_files <- sub(
    "^\\[\\[\\s*Media\\s*:\\s*",
    "",
    media_matches,
    perl = TRUE,
    ignore.case = TRUE
  )

  gallery_files <- .extract_gallery_files(wikitext)

  list(
    embedded = unique(.as_file_title(c(embedded, gallery_files))),
    linked = unique(.as_file_title(c(linked_files, media_files)))
  )
}

.extract_gallery_files <- function(wikitext) {
  blocks <- .extract_regex_matches(
    wikitext,
    "(?is)<gallery\\b[^>]*>.*?</gallery>"
  )

  if (length(blocks) == 0L) {
    return(character())
  }

  files <- unlist(
    lapply(
      blocks,
      function(block) {
        block <- sub("(?is)^<gallery\\b[^>]*>", "", block, perl = TRUE)
        block <- sub("(?is)</gallery>$", "", block, perl = TRUE)
        lines <- trimws(strsplit(block, "\\r?\\n", perl = TRUE)[[1L]])
        lines <- lines[nzchar(lines)]
        lines <- lines[!grepl("^<!--", lines)]
        lines <- sub("\\|.*$", "", lines)
        sub(
          "^(?:File|Image)\\s*:\\s*",
          "",
          lines,
          ignore.case = TRUE,
          perl = TRUE
        )
      }
    ),
    use.names = FALSE
  )

  unique(files[nzchar(files)])
}

.get_file_metadata <- function(file_titles, handle = NULL) {
  file_titles <- unique(.as_file_title(file_titles))

  if (length(file_titles) == 0L) {
    return(.empty_file_metadata())
  }

  batches <- split(
    file_titles,
    ceiling(seq_along(file_titles) / 50L)
  )

  result <- lapply(
    batches,
    function(batch) {
      tryCatch(
        .get_file_metadata_batch(batch, handle = handle),
        error = function(error) {
          do.call(
            rbind,
            lapply(
              batch,
              .unresolved_file_metadata,
              error = conditionMessage(error)
            )
          )
        }
      )
    }
  )

  result <- do.call(rbind, result)
  rownames(result) <- NULL
  result
}

.get_file_metadata_batch <- function(file_titles, handle = NULL) {
  response <- appropedia_query(
    list(
      action = "query",
      titles = paste(file_titles, collapse = "|"),
      prop = "imageinfo",
      iiprop = "url|mime|size|sha1|timestamp",
      redirects = 1,
      format = "json",
      formatversion = 2
    ),
    handle = handle
  )

  mappings <- .title_mappings(response$query)
  pages <- response$query$pages
  page_keys <- .file_key(vapply(pages, `[[`, character(1), "title"))

  rows <- lapply(
    file_titles,
    function(file_title) {
      resolved_title <- .resolve_mapped_title(file_title, mappings)
      page_index <- match(.file_key(resolved_title), page_keys)

      if (is.na(page_index)) {
        return(.unresolved_file_metadata(
          file_title,
          "File metadata were not returned by the Appropedia API."
        ))
      }

      api_page <- pages[[page_index]]
      imageinfo <- api_page$imageinfo

      if (isTRUE(api_page$missing) || is.null(imageinfo)) {
        return(.unresolved_file_metadata(
          file_title,
          "File is missing or unavailable from the configured repository."
        ))
      }

      info <- imageinfo[[1L]]
      canonical_title <- api_page$title
      extension <- tolower(tools::file_ext(
        sub("^File:", "", canonical_title, ignore.case = TRUE)
      ))

      data.frame(
        file_title = file_title,
        canonical_file_title = canonical_title,
        file_url = .first_non_null(info$url, NA_character_),
        description_url = .first_non_null(
          info$descriptionurl,
          NA_character_
        ),
        repository = .first_non_null(
          api_page$imagerepository,
          NA_character_
        ),
        mime_type = .first_non_null(info$mime, NA_character_),
        extension = extension,
        size = as.numeric(.first_non_null(info$size, NA_real_)),
        sha1 = .first_non_null(info$sha1, NA_character_),
        file_timestamp = .first_non_null(info$timestamp, NA_character_),
        asset_class = .classify_asset(
          .first_non_null(info$mime, NA_character_),
          extension
        ),
        status = "ok",
        error = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, rows)
}

.classify_asset <- function(mime_type, extension) {
  image_extensions <- c(
    "avif", "bmp", "gif", "heic", "heif", "jpeg", "jpg", "png",
    "svg", "tif", "tiff", "webp"
  )

  manufacturing_extensions <- c(
    "3dm", "3mf", "amf", "blend", "dxf", "dwg", "fcstd", "gcode",
    "iges", "igs", "obj", "scad", "step", "stl", "stp"
  )

  document_extensions <- c(
    "csv", "doc", "docx", "epub", "ods", "odt", "odp", "pdf", "ppt",
    "pptx", "rtf", "txt", "xls", "xlsx"
  )

  if (!is.na(mime_type) && grepl("^image/", mime_type)) {
    return("image")
  }

  if (extension %in% image_extensions) {
    return("image")
  }

  if (extension %in% manufacturing_extensions) {
    return("manufacturing")
  }

  if (extension %in% document_extensions) {
    return("document")
  }

  "other"
}

.title_mappings <- function(query) {
  mappings <- list()

  entries <- c(query$normalized, query$redirects)

  if (length(entries) == 0L) {
    return(mappings)
  }

  for (entry in entries) {
    mappings[[.file_key(entry$from)]] <- entry$to
  }

  mappings
}

.resolve_mapped_title <- function(title, mappings) {
  current <- title
  visited <- character()

  repeat {
    key <- .file_key(current)

    if (key %in% visited || is.null(mappings[[key]])) {
      break
    }

    visited <- c(visited, key)
    current <- mappings[[key]]
  }

  current
}

.as_file_title <- function(title) {
  title <- trimws(title)
  title <- gsub("_", " ", title, fixed = TRUE)

  missing <- is.na(title)
  has_namespace <- !missing & grepl(
    "^(?:File|Image|Media)\\s*:",
    title,
    ignore.case = TRUE,
    perl = TRUE
  )

  add_namespace <- !missing & !has_namespace
  title[add_namespace] <- paste0("File:", title[add_namespace])
  title <- sub(
    "^(?:Image|Media)\\s*:",
    "File:",
    title,
    ignore.case = TRUE,
    perl = TRUE
  )
  title <- sub("^File\\s*:\\s*", "File:", title, ignore.case = TRUE)
  title
}

.file_key <- function(title) {
  ifelse(
    is.na(title),
    NA_character_,
    tolower(gsub("[ _]+", " ", trimws(.as_file_title(title))))
  )
}

.extract_regex_matches <- function(text, pattern) {
  matches <- gregexpr(
    pattern,
    text,
    perl = TRUE,
    ignore.case = TRUE
  )

  result <- regmatches(text, matches)[[1L]]

  if (length(result) == 1L && identical(result, character(0))) {
    return(character())
  }

  result
}

.combine_errors <- function(existing, added) {
  mapply(
    function(left, right) {
      values <- unique(c(left, right))
      values <- values[!is.na(values) & nzchar(values)]

      if (length(values) == 0L) {
        return(NA_character_)
      }

      paste(values, collapse = " | ")
    },
    existing,
    added,
    USE.NAMES = FALSE
  )
}

.first_non_null <- function(...) {
  values <- list(...)

  for (value in values) {
    if (!is.null(value)) {
      return(value)
    }
  }

  NULL
}

.page_error_row <- function(context) {
  data.frame(
    page_title = context$page_title,
    page_id = context$page_id,
    page_revision_id = context$page_revision_id,
    page_revision_timestamp = context$page_revision_timestamp,
    file_title = NA_character_,
    canonical_file_title = NA_character_,
    file_url = NA_character_,
    description_url = NA_character_,
    repository = NA_character_,
    mime_type = NA_character_,
    extension = NA_character_,
    size = NA_real_,
    sha1 = NA_character_,
    file_timestamp = NA_character_,
    asset_class = NA_character_,
    is_embedded = FALSE,
    is_linked = FALSE,
    is_transcluded = FALSE,
    status = context$status,
    error = context$error,
    stringsAsFactors = FALSE
  )
}

.unresolved_file_metadata <- function(file_title, error) {
  data.frame(
    file_title = file_title,
    canonical_file_title = NA_character_,
    file_url = NA_character_,
    description_url = NA_character_,
    repository = NA_character_,
    mime_type = NA_character_,
    extension = tolower(tools::file_ext(
      sub("^File:", "", file_title, ignore.case = TRUE)
    )),
    size = NA_real_,
    sha1 = NA_character_,
    file_timestamp = NA_character_,
    asset_class = NA_character_,
    status = "unresolved",
    error = error,
    stringsAsFactors = FALSE
  )
}

.empty_file_metadata <- function() {
  data.frame(
    file_title = character(),
    canonical_file_title = character(),
    file_url = character(),
    description_url = character(),
    repository = character(),
    mime_type = character(),
    extension = character(),
    size = numeric(),
    sha1 = character(),
    file_timestamp = character(),
    asset_class = character(),
    status = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
}

.empty_page_assets <- function() {
  data.frame(
    page_title = character(),
    page_id = integer(),
    page_revision_id = integer(),
    page_revision_timestamp = character(),
    file_title = character(),
    canonical_file_title = character(),
    file_url = character(),
    description_url = character(),
    repository = character(),
    mime_type = character(),
    extension = character(),
    size = numeric(),
    sha1 = character(),
    file_timestamp = character(),
    asset_class = character(),
    is_embedded = logical(),
    is_linked = logical(),
    is_transcluded = logical(),
    status = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
}

.get_page_export_content <- function(
    page,
    include_wikitext,
    include_html,
    handle = NULL
) {
  properties <- c(
    if (include_html) "text",
    if (include_wikitext) "wikitext",
    "revid"
  )

  response <- appropedia_query(
    list(
      action = "parse",
      page = page,
      prop = paste(properties, collapse = "|"),
      redirects = 1,
      disableeditsection = 1,
      format = "json",
      formatversion = 2
    ),
    handle = handle
  )

  parsed <- response$parse

  if (is.null(parsed)) {
    stop(sprintf("No page content was returned for: %s", page), call. = FALSE)
  }

  list(
    requested_title = page,
    page_title = if (is.null(parsed$title)) page else parsed$title,
    page_id = as.integer(if (is.null(parsed$pageid)) NA_integer_ else parsed$pageid),
    page_revision_id = as.integer(
      if (is.null(parsed$revid)) NA_integer_ else parsed$revid
    ),
    wikitext = if (include_wikitext) {
      .extract_api_text(parsed$wikitext)
    } else {
      NA_character_
    },
    html = if (include_html) {
      .extract_api_text(parsed$text)
    } else {
      NA_character_
    },
    status = "ok",
    error = NA_character_
  )
}

.prepare_page_directories <- function(page_content) {
  page_titles <- vapply(page_content, `[[`, character(1), "page_title")
  page_ids <- vapply(page_content, function(page) page$page_id, integer(1))
  requested_titles <- vapply(
    page_content,
    `[[`,
    character(1),
    "requested_title"
  )

  base_directories <- vapply(
    page_titles,
    .safe_path_component,
    character(1)
  )

  page_directories <- make.unique(base_directories, sep = "__")

  data.frame(
    requested_title = requested_titles,
    page_title = page_titles,
    page_id = page_ids,
    page_directory = page_directories,
    stringsAsFactors = FALSE
  )
}

.write_page_content <- function(
    page_content,
    page_table,
    package_path,
    include_wikitext,
    include_html,
    overwrite
) {
  rows <- list()
  row_index <- 0L

  for (index in seq_along(page_content)) {
    content <- page_content[[index]]
    page_row <- page_table[index, , drop = FALSE]
    page_filename <- .safe_path_component(content$page_title)

    if (include_wikitext) {
      row_index <- row_index + 1L
      destination <- file.path(
        package_path,
        page_row$page_directory,
        "wikitext",
        paste0(page_filename, ".wiki")
      )

      rows[[row_index]] <- .write_content_item(
        page_title = content$page_title,
        page_id = content$page_id,
        page_revision_id = content$page_revision_id,
        item_type = "wikitext",
        source_title = content$page_title,
        source_url = .page_source_url(content$page_title),
        destination = destination,
        package_path = package_path,
        content = content$wikitext,
        source_status = content$status,
        source_error = content$error,
        overwrite = overwrite
      )
    }

    if (include_html) {
      row_index <- row_index + 1L
      destination <- file.path(
        package_path,
        page_row$page_directory,
        "html",
        paste0(page_filename, ".html")
      )

      html <- if (!is.na(content$html)) {
        .wrap_page_html(
          title = content$page_title,
          html = content$html,
          source_url = .page_source_url(content$page_title),
          revision_id = content$page_revision_id
        )
      } else {
        NA_character_
      }

      rows[[row_index]] <- .write_content_item(
        page_title = content$page_title,
        page_id = content$page_id,
        page_revision_id = content$page_revision_id,
        item_type = "html",
        source_title = content$page_title,
        source_url = .page_source_url(content$page_title),
        destination = destination,
        package_path = package_path,
        content = html,
        source_status = content$status,
        source_error = content$error,
        overwrite = overwrite
      )
    }
  }

  if (length(rows) == 0L) {
    return(.empty_download_report())
  }

  do.call(rbind, rows)
}

.write_content_item <- function(
    page_title,
    page_id,
    page_revision_id,
    item_type,
    source_title,
    source_url,
    destination,
    package_path,
    content,
    source_status,
    source_error,
    overwrite
) {
  if (!identical(source_status, "ok") || is.na(content)) {
    return(.download_report_row(
      page_title = page_title,
      page_id = page_id,
      page_revision_id = page_revision_id,
      item_type = item_type,
      asset_class = NA_character_,
      source_title = source_title,
      source_url = source_url,
      local_path = .relative_path(destination, package_path),
      sha1 = NA_character_,
      source_status = source_status,
      download_status = "failed",
      http_status = NA_integer_,
      bytes = NA_real_,
      error = source_error
    ))
  }

  if (file.exists(destination) && !overwrite) {
    return(.download_report_row(
      page_title = page_title,
      page_id = page_id,
      page_revision_id = page_revision_id,
      item_type = item_type,
      asset_class = NA_character_,
      source_title = source_title,
      source_url = source_url,
      local_path = .relative_path(destination, package_path),
      sha1 = NA_character_,
      source_status = source_status,
      download_status = "skipped_existing",
      http_status = NA_integer_,
      bytes = unname(file.info(destination)$size),
      error = NA_character_
    ))
  }

  result <- tryCatch(
    {
      writeLines(enc2utf8(content), destination, useBytes = TRUE)

      list(
        status = "written",
        bytes = unname(file.info(destination)$size),
        error = NA_character_
      )
    },
    error = function(error) {
      list(
        status = "failed",
        bytes = NA_real_,
        error = conditionMessage(error)
      )
    }
  )

  .download_report_row(
    page_title = page_title,
    page_id = page_id,
    page_revision_id = page_revision_id,
    item_type = item_type,
    asset_class = NA_character_,
    source_title = source_title,
    source_url = source_url,
    local_path = .relative_path(destination, package_path),
    sha1 = NA_character_,
    source_status = source_status,
    download_status = result$status,
    http_status = NA_integer_,
    bytes = result$bytes,
    error = result$error
  )
}

.download_content_assets <- function(
    assets,
    page_table,
    package_path,
    overwrite
) {
  if (nrow(assets) == 0L) {
    return(.empty_download_report())
  }

  assets <- assets[!is.na(assets$file_title), , drop = FALSE]

  if (nrow(assets) == 0L) {
    return(.empty_download_report())
  }

  page_indices <- .match_assets_to_pages(assets, page_table)

  unmatched <- is.na(page_indices)

  if (any(unmatched)) {
    unmatched_pages <- unique(assets$page_title[unmatched])
    stop(
      sprintf(
        "Some asset rows could not be matched to `pages`: %s",
        paste(unmatched_pages, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  filenames <- .prepare_asset_filenames(assets, page_indices)
  rows <- vector("list", nrow(assets))

  for (index in seq_len(nrow(assets))) {
    asset <- assets[index, , drop = FALSE]
    page_index <- page_indices[index]
    asset_directory <- .asset_directory(asset$asset_class)
    destination <- file.path(
      package_path,
      page_table$page_directory[page_index],
      asset_directory,
      filenames[index]
    )

    unresolved <- asset$status %in% c("unresolved", "page_error") ||
      is.na(asset$file_url) || !nzchar(asset$file_url)

    if (unresolved) {
      rows[[index]] <- .download_report_row(
        page_title = asset$page_title,
        page_id = asset$page_id,
        page_revision_id = asset$page_revision_id,
        item_type = "asset",
        asset_class = asset$asset_class,
        source_title = if (is.na(asset$canonical_file_title)) {
          asset$file_title
        } else {
          asset$canonical_file_title
        },
        source_url = asset$file_url,
        local_path = .relative_path(destination, package_path),
        sha1 = asset$sha1,
        source_status = asset$status,
        download_status = "unresolved",
        http_status = NA_integer_,
        bytes = NA_real_,
        error = asset$error
      )
      next
    }

    result <- .download_asset_file(
      url = asset$file_url,
      destination = destination,
      overwrite = overwrite
    )

    rows[[index]] <- .download_report_row(
      page_title = asset$page_title,
      page_id = asset$page_id,
      page_revision_id = asset$page_revision_id,
      item_type = "asset",
      asset_class = asset$asset_class,
      source_title = if (is.na(asset$canonical_file_title)) {
        asset$file_title
      } else {
        asset$canonical_file_title
      },
      source_url = asset$file_url,
      local_path = .relative_path(destination, package_path),
      sha1 = asset$sha1,
      source_status = asset$status,
      download_status = result$status,
      http_status = result$http_status,
      bytes = result$bytes,
      error = result$error
    )
  }

  do.call(rbind, rows)
}

.download_asset_file <- function(
    url,
    destination,
    overwrite,
    debug = FALSE
) {
  if (file.exists(destination) && !overwrite) {
    return(list(
      status = "skipped_existing",
      http_status = NA_integer_,
      bytes = unname(file.info(destination)$size),
      error = NA_character_
    ))
  }

  temporary_file <- tempfile(
    pattern = ".appropedia-download-",
    tmpdir = dirname(destination)
  )

  on.exit(unlink(temporary_file), add = TRUE)

  tryCatch({
    if (debug) {
      message("Request URL: ", url)
      message("Destination: ", destination)
    }

    response <- httr::GET(
      url,
      httr::user_agent("appropedia R package"),
      httr::add_headers(
        Referer = "https://www.appropedia.org/",
        Accept = "*/*"
      ),
      httr::write_disk(temporary_file, overwrite = TRUE),
      if (debug) httr::verbose() else NULL
    )

    status <- httr::status_code(response)

    if (debug) {
      message("Final URL: ", response$url)
      message("HTTP status: ", status)
      message("Response headers:")
      print(httr::headers(response))

      if (file.exists(temporary_file)) {
        message("Temporary file bytes: ", file.info(temporary_file)$size)

        preview <- tryCatch(
          readLines(temporary_file, n = 20L, warn = FALSE),
          error = function(e) character()
        )

        if (length(preview)) {
          message("Response body preview:\n", paste(preview, collapse = "\n"))
        }
      }
    }

    httr::stop_for_status(response)

    if (file.exists(destination)) {
      unlink(destination)
    }

    if (!file.rename(temporary_file, destination)) {
      if (!file.copy(temporary_file, destination, overwrite = TRUE)) {
        stop("Downloaded file could not be moved into the package.")
      }
    }

    list(
      status = "downloaded",
      http_status = status,
      bytes = unname(file.info(destination)$size),
      error = NA_character_
    )
  }, error = function(error) {
    list(
      status = "failed",
      http_status = if (exists("status")) status else NA_integer_,
      bytes = NA_real_,
      error = conditionMessage(error)
    )
  })
}

.validate_content_package_assets <- function(assets) {
  if (!is.data.frame(assets)) {
    stop(
      "`assets` must be a data frame returned by `collect_page_assets()`.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "page_title",
    "page_id",
    "page_revision_id",
    "file_title",
    "canonical_file_title",
    "file_url",
    "asset_class",
    "sha1",
    "status",
    "error"
  )

  missing_columns <- setdiff(required_columns, names(assets))

  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "`assets` is missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(assets)
}

.match_assets_to_pages <- function(assets, page_table) {
  result <- rep(NA_integer_, nrow(assets))

  has_id <- !is.na(assets$page_id)
  result[has_id] <- match(assets$page_id[has_id], page_table$page_id)

  needs_title <- is.na(result)

  if (any(needs_title)) {
    result[needs_title] <- match(
      .page_key(assets$page_title[needs_title]),
      .page_key(page_table$page_title)
    )
  }

  result
}

.prepare_asset_filenames <- function(assets, page_indices) {
  titles <- ifelse(
    is.na(assets$canonical_file_title),
    assets$file_title,
    assets$canonical_file_title
  )

  titles <- sub("^File\\s*:\\s*", "", titles, ignore.case = TRUE)
  filenames <- vapply(titles, .safe_path_component, character(1))
  groups <- paste(page_indices, .asset_directory(assets$asset_class), sep = "::")

  for (group in unique(groups)) {
    indices <- which(groups == group)
    filenames[indices] <- .make_unique_filenames(filenames[indices])
  }

  filenames
}

.make_unique_filenames <- function(filenames) {
  result <- filenames
  used <- character()

  for (index in seq_along(filenames)) {
    candidate <- filenames[index]
    key <- tolower(candidate)

    if (!key %in% used) {
      used <- c(used, key)
      next
    }

    extension <- tools::file_ext(candidate)
    stem <- if (nzchar(extension)) {
      substr(candidate, 1L, nchar(candidate) - nchar(extension) - 1L)
    } else {
      candidate
    }

    suffix <- 2L

    repeat {
      alternative <- if (nzchar(extension)) {
        paste0(stem, "__", suffix, ".", extension)
      } else {
        paste0(stem, "__", suffix)
      }

      alternative_key <- tolower(alternative)

      if (!alternative_key %in% used) {
        result[index] <- alternative
        used <- c(used, alternative_key)
        break
      }

      suffix <- suffix + 1L
    }
  }

  result
}

.asset_directory <- function(asset_class) {
  asset_class <- ifelse(is.na(asset_class), "other", asset_class)

  unname(c(
    image = "images",
    manufacturing = "manufacturing",
    document = "documents",
    other = "other"
  )[ifelse(asset_class %in% c(
    "image",
    "manufacturing",
    "document",
    "other"
  ), asset_class, "other")])
}

.safe_path_component <- function(value) {
  value <- enc2utf8(value)
  value <- gsub("[<>:\"/\\\\|?*]", "_", value, perl = TRUE)
  value <- gsub("[[:cntrl:]]", "_", value)
  value <- gsub("[[:space:]]+", " ", value)
  value <- trimws(value)
  value <- sub("[. ]+$", "", value)

  if (!nzchar(value)) {
    value <- "untitled"
  }

  reserved <- c(
    "CON", "PRN", "AUX", "NUL",
    paste0("COM", 1:9),
    paste0("LPT", 1:9)
  )

  stem <- toupper(sub("\\..*$", "", value))

  if (stem %in% reserved) {
    value <- paste0("_", value)
  }

  value
}

.page_key <- function(title) {
  tolower(gsub("[ _]+", " ", trimws(title)))
}

.extract_api_text <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }

  if (is.character(value)) {
    return(value[[1L]])
  }

  if (is.list(value)) {
    candidates <- c("content", "*")

    for (candidate in candidates) {
      if (!is.null(value[[candidate]])) {
        return(as.character(value[[candidate]])[[1L]])
      }
    }
  }

  NA_character_
}

.page_source_url <- function(page_title) {
  base_url <- .appropedia_base_url()
  encoded_title <- utils::URLencode(
    gsub(" ", "_", page_title, fixed = TRUE),
    reserved = TRUE
  )

  paste0(base_url, encoded_title)
}

.appropedia_base_url <- function() {
  parsed <- httr::parse_url(get_appropedia_api_url())
  parsed$path <- "/"
  parsed$query <- NULL
  parsed$fragment <- NULL
  httr::build_url(parsed)
}

.wrap_page_html <- function(title, html, source_url, revision_id) {
  revision_text <- if (!is.na(revision_id)) {
    paste0(" revision ", revision_id)
  } else {
    ""
  }

  paste0(
    "<!doctype html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "  <meta charset=\"utf-8\">\n",
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "  <base href=\"", .escape_html(.appropedia_base_url()), "\">\n",
    "  <title>", .escape_html(title), "</title>\n",
    "</head>\n",
    "<body>\n",
    "<!-- Exported from ", .escape_html(source_url), revision_text, " -->\n",
    html,
    "\n</body>\n",
    "</html>\n"
  )
}

.escape_html <- function(value) {
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value
}


.relative_path <- function(path, root) {
  normalized_path <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
  normalized_root <- normalizePath(
    root,
    winslash = "/",
    mustWork = TRUE
  )

  prefix <- paste0(normalized_root, "/")

  if (startsWith(normalized_path, prefix)) {
    substring(normalized_path, nchar(prefix) + 1L)
  } else {
    normalized_path
  }
}

.download_report_row <- function(
    page_title,
    page_id,
    page_revision_id,
    item_type,
    asset_class,
    source_title,
    source_url,
    local_path,
    sha1,
    source_status,
    download_status,
    http_status,
    bytes,
    error
) {
  data.frame(
    page_title = as.character(page_title),
    page_id = as.integer(page_id),
    page_revision_id = as.integer(page_revision_id),
    item_type = as.character(item_type),
    asset_class = as.character(asset_class),
    source_title = as.character(source_title),
    source_url = as.character(source_url),
    local_path = as.character(local_path),
    sha1 = as.character(sha1),
    source_status = as.character(source_status),
    download_status = as.character(download_status),
    http_status = as.integer(http_status),
    bytes = as.numeric(bytes),
    error = as.character(error),
    stringsAsFactors = FALSE
  )
}

.empty_download_report <- function() {
  data.frame(
    page_title = character(),
    page_id = integer(),
    page_revision_id = integer(),
    item_type = character(),
    asset_class = character(),
    source_title = character(),
    source_url = character(),
    local_path = character(),
    sha1 = character(),
    source_status = character(),
    download_status = character(),
    http_status = integer(),
    bytes = numeric(),
    error = character(),
    stringsAsFactors = FALSE
  )
}
