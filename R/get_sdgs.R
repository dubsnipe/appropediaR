# require(tidyverse)
# require(data.table)
#
# temp <- tempfile()
# options(timeout = 600) # 10 minutes
# download.file("https://downloads.cs.stanford.edu/nlp/data/wordvecs/glove.2024.wikigiga.100d.zip",
#               destfile = temp,
#               mode = "wb")
# zip_list <- unzip(temp, list = TRUE)
# zip_file <- zip_list[1,]$Name
#
# unzip(zipfile = temp, files = zip_file)
#
# embeddings <- fread(zip_file,
#                     header = FALSE,
#                     sep = " ",
#                     quote = "",
#                     nThread = 4
#                     )
# glove_matrix <- as.matrix(embeddings[, -1, with = FALSE])
# rownames(glove_matrix) <- embeddings[[1]]  # first column = words
#
# unlink(temp)
#
# # First column = word, rest = vectors
# words <- embeddings[[1]]
# matrix <- as.matrix(embeddings[, -1, with = FALSE])
# rownames(matrix) <- words
#
# sentence_embedding <- function(sentence, vectors) {
#   words <- strsplit(tolower(sentence), "\\s+")[[1]]
#   valid <- words[words %in% rownames(vectors)]
#   if (length(valid) == 0) return(rep(0, ncol(vectors)))
#   colMeans(vectors[valid, , drop = FALSE])
# }
#
# metadata_slim <- all_content %>%
#   select( Title, Aliases, Categories, Description, Keywords, Project_type,
#           Organizations, Project_uses, Project_description,
#           Derivative_of, Project_tools ) %>%
#   mutate(title = Title,
#          text = paste( Description, Categories, Aliases, Keywords,
#                        Project_type, Organizations,
#                        Project_uses, Project_description,
#                        Derivative_of, Project_tools ) ) %>%
#   select(title, text) %>%
#   mutate(text = str_replace_all(text, "NA", ""))
# metadata_slim$embedding <- lapply(metadata_slim$text, sentence_embedding, vectors = glove_matrix)
# metadata_slim$SDG <- all_content$SDG
#
# sdg_centroids <- lapply(split(metadata_slim, metadata_slim$SDG), function(df) {
#   mat <- do.call(rbind, df$embedding)
#   colMeans(mat, na.rm = TRUE)
# })
#
# sdg_ref <- c(
#   "SDG 1 is to end poverty in all its forms everywhere.
#       extreme poverty
#       social protection systems
#       rights ownership, basic services, technology, and economic resources
#       Build resilience to environmental, economic, and social disasters",
#   "SDG 2 is End hunger, achieve food security, improved nutrition
#       Universal access to safe and nutritious food
#       malnutrition
#       productivity and incomes of small-scale food producers
#       Sustainable food production and resilient agricultural practices
#       genetic diversity in food production",
#   "SDG 3 Ensure healthy lives and promote well-being for all at all ages
#       maternal mortality
#       preventable deaths children
#       communicable diseases
#       mortality from non-communicable diseases and promote mental health
#       Prevent and treat substance abuse
#       road injuries and deaths
#       Universal access to sexual and reproductive care, family planning and education
#       universal health coverage
#       Reduce illnesses and deaths from hazardous chemicals and pollution",
#   "SDG 4 Ensure inclusive and equitable quality education and promote
#       Free primary and secondary education
#       quality pre-primary education
#       Equal access to affordable technical, vocational, and higher education
#       Increase the number of people with relevant skills for financial success
#       Eliminate all discrimination in education
#       Universal literacy and numeracy
#       Education for sustainable development and global citizenship",
#   "SDG 5 Achieve gender equality and empower all women and girls
#       End discrimination against women and girls
#       End all violence against and exploitation of women and girls
#       Eliminate forced marriages and genital mutilation
#       Value unpaid care and promote shared domestic responsibilities
#       Ensure full participation in leadership and decision-making
#       Universal access to reproductive rights and health",
#   "SDG 6 Ensure availability and sustainable management of water and sanitation for all
#       Safe and affordable drinking water
#       End open defecation and provide access to sanitation and hygiene
#       Drinking water
#       Improve water quality, wastewater treatment, and safe reuse
#       Increase water-use efficiency and ensure fresh water supplies
#       Protect and restore water-related ecosystems",
#   "SDG 7 Ensure access to affordable, reliable, sustainable and modern energy for all
#       Universal access to modern energy
#       Increase global percentage of renewable energy
#       Double the improvement in energy efficiency",
#   "SDG 8 Promote sustained, inclusive and sustainable economic growth,
#       Sustainable economic growth
#       Diversify, innovate and upgrade for economic productivity
#       Promote policies to support job creation and growing enterprises
#       Improve resource efficiency in consumption and production
#       Full employment and decent work with equal pay
#       Promote youth employment, education and training
#       End modern slavery, trafficking, and child labour
#       Protect labour rights and promote safe working environments
#       Promote beneficial and sustainable tourism
#       Universal access to banking, insurance and financial services",
#   "SDG 9 Build resilient infrastructure, promote inclusive and sustainable
#       Develop sustainable, resilient and inclusive infrastructures
#       Promote inclusive and sustainable industrialization
#       Increase access to financial services and markets
#       Upgrade all industries and infrastructures for sustainability
#       Enhance research and upgrade industrial technologies",
#   "SDG10 Reduce inequality within and among countries
#       Reduce income inequalities
#       Promote universal social, economic and political inclusion
#       Ensure equal opportunities and end discrimination
#       Adopt fiscal and social policies that promote equality
#       Improved regulation of global financial markets and institutions
#       Enhanced representation for developing countries in financial institutions
#       Responsible and well-managed migration policies",
#   "SDG11 Make cities and human settlements inclusive, safe, resilient and sustainable
#       Safe and affordable housing
#       Affordable and sustainable transport systems
#       Inclusive and sustainable urbanization
#       Protect the world's cultural and natural heritage
#       Reduce the adverse effects of natural disasters
#       Reduce the environmental impacts of cities
#       Provide access to safe and inclusive green and public spaces",
#   "SDG12 Ensure sustainable consumption and production patterns
#     Implement the 10-year sustainable consumption and production framework
#     Sustainable management and use of natural resources
#     Halve global per capita food waste
#     Responsible management of chemicals and waste
#     Substantially reduce waste generation
#     Encourage companies to adopt sustainable practices and sustainability reporting
#     Promote sustainable public procurement practices
#     Promote universal understanding of sustainable lifestyles",
#   "SDG13 Take urgent action to combat climate change and its impacts
#       Strengthen resilience and adaptive capacity to climate-related disasters
#       Integrate climate change measures into policy and planning
#       Build knowledge and capacity to meet climate change",
#     "SDG14 Conserve and sustainably use the oceans, seas and marine resources for sustainable development
#       Reduce marine pollution
#       Marine pollution from plastics
#       Protect and restore ecosystems
#       Reduce ocean acidification
#       Sustainable fishing
#       Conserve coastal and marine areas
#       End subsidies contributing to overfishing
#       Increase the economic benefits from sustainable use of marine resources
#       Non-living resources of the ocean seabed mining",
#     "SDG15 Protect, restore and promote sustainable use of terrestrial ecosystems, sustainably manage forests, combat desertification, and halt and reverse land degradation and halt biodiversity loss
#       Conserve and restore terrestrial and freshwater ecosystems
#       End deforestation and restore degraded forests
#       End desertification and restore degraded land
#       Ensure conservation of mountain ecosystems
#       Protect biodiversity and natural habitats
#       Protect access to genetic resources and fair sharing of the benefits
#       Eliminate poaching and trafficking of protected species
#       Prevent invasive alien species on land and in water ecosystems
#       Integrate ecosystem and biodiversity in governmental planning",
#   "SDG16 Promote peaceful and inclusive societies for sustainable development, provide access to justice for all and build effective, accountable and inclusive institutions at all levels
#     Significantly reduce all forms of violence and related death rates everywhere
#     End abuse, exploitation, trafficking and all forms of violence against and torture of children
#     Promote the rule of law at the national and international levels and ensure equal access to justice for all
#     reduce illicit financial and arms flows, strengthen the recovery and return of stolen assets and combat all forms of organized crime.
#     Substantially reduce corruption and bribery in all their forms
#     Develop effective, accountable and transparent institutions at all levels
#     Ensure responsive, inclusive, participatory and representative decision-making at all levels
#     Broaden and strengthen the participation of developing countries in the institutions of global governance.
#     legal identity for all, including birth registration
#     Ensure public access to information and protect fundamental freedoms",
#   "SDG17 Strengthen the means of implementation and revitalize the Global Partnership for Sustainable Development
#     mobilize resources to improve domestic revenue collection
#     Implement all development assistance commitments
#     Mobilize financial resources for developing countries
#     Assist developing countries in attaining debt sustainability
#     Invest in least-developed countries
#     Knowledge sharing and cooperation for access to science, technology and innovation
#     Promote sustainable technologies to developing countries
#     Strengthen the science, technology and innovation capacity for least-developed countries
#     Enhanced SDG capacity in developing countries
#     Promote a universal trading system under the WTO
#     Increase the exports of developing countries
#     Remove trade barriers for least-developed countries
#     Enhance global macroeconomic stability
#     Enhance policy coherence for sustainable development
#     Respect national leadership to implement policies for the sustainable development goals
#     Enhance the global partnership for sustainable development
#     Encourage effective partnerships
#     Enhance availability of reliable data
#     Further develop measurements of progress"
# )
#
# sdg_ref_embed <- lapply(sdg_ref, sentence_embedding, vectors = glove_matrix)
# names(sdg_ref_embed) <- paste0("SDG", 1:17)
# cosine_sim <- function(a, b) {
#   sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
# }
#
# # having the same names for both
# names(sdg_centroids) <- paste("SDG", str_pad(1:17, 2, "left", "0"))
# names(sdg_ref_embed) <- paste("SDG", str_pad(1:17, 2, "left", "0"))
#
# predict_sdg <- function(embedding, centroids, refs, w_internal = 0.7) {
#   sims_internal <- sapply(centroids, cosine_sim, a = embedding)
#   sims_external <- sapply(refs, cosine_sim, a = embedding)
#
#   # match names between centroids and refs (make sure consistent)
#   common <- intersect(names(sims_internal), names(sims_external))
#
#   combined <- w_internal * sims_internal[common] +
#     (1 - w_internal) * sims_external[common]
#
#   sort(combined, decreasing = TRUE)
# }
#
#
# get_prediction <- function(embeding_vec, threshold = 0.9, results = 3){
#   p <- predict_sdg(embeding_vec, sdg_centroids, sdg_ref_embed)
#   p_above <- p[p > threshold]
#   cat("Passing:", length(p_above), "\n")
#   p_top <- head(p_above ,results)
#   p_names <- paste(names(p_top), collapse = ", ")
#   return(p_names)
# }
#
# predictions <- list()
# predictions <- unlist(lapply(metadata_slim$embedding, get_prediction))
#
# metadata_slim$sdg_recs <- predictions
#
#
