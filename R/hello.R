# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

if (interactive()) {
  require("devtools", quietly = TRUE)

  library(httr)
  library(readr)
  library(dplyr)
  library(stringr)
  require(lubridate)
  require(jsonlite)

  # automatically attaches usethis
}



# write_file <- function(x, type = "output") {
#   # Determine size
#   if (is.vector(x)) {
#     list_size <- length(x)
#     x_to_write <- data.frame(Value = x)  # turn vector into one-column data frame
#   } else if (is.data.frame(x)) {
#     list_size <- nrow(x)
#     x_to_write <- x
#   } else {
#     stop("Input must be a vector or a data frame")
#   }
#
#   # Create safe filename
#   filename <- paste0(type, " (", Sys.Date(), " - ", list_size, " rows).csv")
#
#   write.csv(x_to_write, filename, row.names = FALSE)
#   cat("Saved", filename, "\n")
# }
#
#
# do_page_checks <- function(){
#   all_size <- length(all_pages)
#   redirects_size <- length(redirects)
#   categories_size <- length(all_categories)
#   metadata_size <- length(all_metadata)
#   sdg_size <- length(metadata_slim$sdg_recs)
#     message <- paste0("all_pages: ", all_size,
#                       " redirects: ", redirects_size,
#                      " all_categories: ", categories_size,
#                      " all_metadata: ", metadata_size,
#                      " sdg_prediction: ", sdg_size)
#   return(message)
# }
#
#
# #### Consolidate
# tmp_content <- tibble(Alias = all_pages,
#                       Title = redirects,
#                       Categories = all_categories3
#                       )
# tmp_content <- tibble(Alias = all_pages, Title = redirects)
#
# # Delete redirect if it's the same value as the Title (should be fixed later)
# tmp_content <- tmp_content %>%
#   mutate(Alias = str_replace_all(Alias, "_", " ")) %>%
#   mutate(Title = str_replace_all(Title, "_", " ")) %>%
#   mutate(Alias = if_else(Alias == Title, NA_character_, Alias)
#          )
#
# # Group all redirection titles as alternative titles
# # consolidate all alt titles in a column associated to each page
# # count the number of alt titles as well
# tmp_grouped <- tmp_content %>%
#   filter(!is.na(Title)) %>%
#   group_by(Title) %>%
#   summarise(
#     Aliases = paste(
#       sapply(unique(Alias[!is.na(Alias)]), function(x) {
#         if (grepl(",", x)) paste0('"', x, '"') else x
#       }),
#       collapse = ", "
#     ),
#     n_Alias = n_distinct(Alias[!is.na(Alias)]),
#     # Categories = paste(unique(Categories), collapse = ", "),
#     # n_Categories = length(unique(unlist(strsplit(Categories[Categories != ""], ",\\s*")))),
#     .groups = "drop"
#   )
# tmp_grouped <- tmp_grouped[!grepl("^Appropedia:", tmp_grouped$Title), ]
# tmp_grouped <- tmp_grouped[!grepl("^Category:", tmp_grouped$Title), ]
# tmp_grouped <- tmp_grouped[!grepl("^Template:", tmp_grouped$Title), ]
# tmp_grouped <- tmp_grouped[!grepl("^User:", tmp_grouped$Title), ]
# tmp_grouped <- tmp_grouped[!grepl("^Help:", tmp_grouped$Title), ]
# tmp_grouped <- tmp_grouped[!grepl("^Appropedia:", tmp_grouped$Title), ]
#
# # Off to get Categories
# all_categories <- collect_all_categories(tmp_grouped$Title)
#
# tmp_grouped$Categories <- all_categories
#
# # Off to clean translations
# translations <- tmp_grouped[grepl("Category:Automatic translations", tmp_grouped$Categories),]
# translations_count <- translations %>% select(Title) %>%
#   mutate(Original = str_replace_all(Title, "(.*)/[a-z]{2}", "\\1")) %>%
#   group_by(Original) %>%
#   summarize(n_Translations = n(), .groups = "drop") %>%
#   select(Title = Original, n_Translations)
#
# no_translations <- tmp_grouped[!grepl("Category:Automatic translations", tmp_grouped$Categories),]
#
# clean_pages <- full_join(no_translations, translations_count)
#
# # Off to get all metadata from pages
# useful_metadata <- unique(all_metadata[all_metadata$Title %in% clean_pages$Title, ])
# missing_metadata_titles <- clean_pages[!clean_pages$Title %in% useful_metadata$Title,]$Title
#
# missing_metadata <- collect_metadata(missing_metadata_titles)
# missing_metadata <- bind_cols(Missing = missing_metadata_titles, missing_metadata)
# missing_metadata2 <- missing_metadata %>% mutate(Title2 = Missing) %>%
#   select(-Missing) %>%
#   filter(!is.na(Title))
# missing_metadata2 <- missing_metadata2[!missing_metadata2[!is.na(missing_metadata2$Title),]$Title %in% tmp_content$Alias,]
# missing_metadata2 <- missing_metadata2 %>% mutate(Title = Title2) %>% select(-Title2)
# missing_metadata2 <- missing_metadata2[!grepl("div class", missing_metadata2$Description),]
#
# all_metadata2 <- bind_rows(missing_metadata2, all_metadata)
# all_content <- full_join(clean_pages, all_metadata2, by = "Title")
# all_content <- all_content[!grepl("^Help:", all_content$Title),]
# all_content <- all_content[!grepl("^Appropedia:", all_content$Title),]
# all_content <- all_content[!grepl("^User:", all_content$Title),]
