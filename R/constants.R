#' constants.R

#' MediaWiki namespace definitions
#'
#' Lookup table containing MediaWiki namespace IDs and their corresponding
#' names.
#'
#'
#' @format A data frame with two columns:
#' \describe{
#' \item{number}{Integer namespace identifier.}
#' \item{name}{Namespace name.}
#' }
#'
#' @source MediaWiki namespace constants: 
#' https://www.mediawiki.org/wiki/Manual:Namespace_constants
#' @examples
#'\dontrun{
#' mediawiki_namespaces
#' }
mediawiki_namespaces <- data.frame(number = 0:15, name = c("",
                                                 "Talk",
                                                 "User",
                                                 "User_talk",
                                                 "Project",
                                                 "Project_talk",
                                                 "File",
                                                 "File_talk",
                                                 "MediaWiki",
                                                 "MediaWiki_talk",
                                                 "Template",
                                                 "Template_talk",
                                                 "Help",
                                                 "Help_talk",
                                                 "Category",
                                                 "Category_talk"))

#' Semantic MediaWiki property schema
#'
#' Named character vector mapping Appropedia Semantic MediaWiki property names
#' to standardized column names used by the package.
#'
#' This object defines the schema used when retrieving semantic metadata with
#' functions such as \code{get_semantic_properties()}.
#'
#' The names of the vector correspond to output column names, while the values
#' correspond to Semantic MediaWiki property names on the wiki.
#'
#' @format A named character vector.
#'
#' @source Appropedia Special:Properties.
#'
#' @examples
#'\dontrun{
#' property_map["Page_title"]
#' names(property_map)
#' }
property_map <- c(
  Device_hardware_license = "Device hardware license",
  Device_software_license = "Device software license",
  Event_date = "Event date",
  Event_date_text = "Event date text",
  Event_keywords = "Event keywords",
  Event_link = "Event link",
  Event_location = "Event location",
  Event_source = "Event source",
  Event_text = "Event text",
  Event_title = "Event title",
  # File_author = "File author",
  # File_date = "File date",
  # File_license = "File license",
  # MaterialHeroImage = "MaterialHeroImage",
  News_date = "News date",
  News_keywords = "News keywords",
  News_link = "News link",
  News_location = "News location",
  News_source = "News source",
  News_text = "News text",
  News_title = "News title",
  Page_SDG = "Page SDG",
  Page_URL = "Page URL",
  Page_affiliations = "Page affiliations",
  Page_authors = "Page authors",
  Page_coordinates = "Page coordinates",
  Page_description = "Page description",
  Page_image = "Page image",
  Page_is_derivative_of = "Page is derivative of",
  Page_is_part_of = "Page is part of",
  Page_is_ported_from = "Page is ported from",
  Page_keywords = "Page keywords",
  Page_language = "Page language",
  Page_license = "Page license",
  Page_location = "Page location",
  Page_parent = "Page parent",
  Page_root = "Page root",
  Page_title = "Page title",
  Page_title_tag = "Page title tag",
  Page_views = "Page views",
  Page_year = "Page year",
  Place_coordinates = "Place coordinates",
  Place_link = "Place link",
  Place_location = "Place location",
  Place_name = "Place name",
  Place_text = "Place text",
  Project_authors = "Project authors",
  Project_cost = "Project cost",
  Project_description = "Project description",
  Project_environment = "Project environment",
  Project_status = "Project status",
  Project_thumb = "Project thumb",
  Project_tools = "Project tools",
  Project_type = "Project type",
  Project_uses = "Project uses",
  Project_was_made = "Project was made",
  Project_was_replicated = "Project was replicated",
  Project_year = "Project year",
  SELF_acting_roles = "SELF acting roles",
  SELF_body_parts = "SELF body parts",
  SELF_body_systems = "SELF body systems",
  SELF_equipment = "SELF equipment",
  SELF_health_classification = "SELF health classification",
  SELF_health_topic = "SELF health topic",
  SELF_interventions = "SELF interventions",
  SELF_pathologies = "SELF pathologies",
  SELF_self_assessment = "SELF self-assessment",
  SELF_subskills = "SELF subskills"
)