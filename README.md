# appropedia

Tools for working with the Appropedia MediaWiki API from R.

The package supports retrieving pages and categories, collecting page metadata, resolving redirects, and making authenticated edits where configured.

## Installation

Install from a local package checkout:

```r
devtools::install()
```

During development:

```r
devtools::load_all()
```

## Configuration

Some functions require Appropedia API settings or bot credentials.

Store required values in your user-level `.Renviron` file rather than directly in scripts or package code. Restart R after editing `.Renviron`.

Authenticated functions will fail if the necessary credentials are not configured.

## Basic use

```r
library(appropedia)

page <- get_page_content("Water")

categories <- get_categories_for_pages(
  c("Water", "Solar cooker")
)
```

## Main capabilities

* Retrieve page content and metadata
* Retrieve pages and categories
* Resolve redirects
* Collect structured metadata from Appropedia pages
* Authenticate with the MediaWiki API
* Edit pages with bot credentials
* Support bulk metadata and description updates

## Development

This package is under active development and is primarily intended for Appropedia data, maintenance, and knowledge-organization workflows.

## License

See `DESCRIPTION` and the repository license file.
