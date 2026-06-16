# Validate category -> redirects -> semantic metadata workflow

rm(list = ls())
devtools::load_all()

category <- "OAXIN"

pages <- get_pages_from_category(category)

pages <- apply_redirects(pages)

# Remove automatic translations
translations <- get_pages_from_category(
  "Automatic translations"
)

pages_not_tl <- pages[
  !(pages %in% translations)
]

metadata <- get_semantic_properties(
  pages_not_tl,
  property_map = property_map
)

stopifnot(nrow(metadata) > 0)

print(metadata)

# Inspect rows containing metadata
metadata[
  rowSums(!is.na(metadata)) > 1,
]