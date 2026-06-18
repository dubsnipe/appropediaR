# Validate category -> redirects -> semantic metadata workflow

rm(list = ls())
devtools::load_all()

category <- "OAXIN"

pages <- get_pages_from_category(category)

translations <- get_pages_from_category("Automatic translations")

redirected_pages <- apply_redirects(pages)

pages_not_tl <- redirected_pages[
  !(redirected_pages %in% translations)
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
