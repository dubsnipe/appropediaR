# Validate template editing logic without saving

rm(list = ls())
devtools::load_all()

page <- "Water"

old_content <- get_page_content(page)

############################################################
# TEST 1: Add a missing parameter
#
# Expected:
# - template_found = TRUE
# - parameter_found = FALSE
# - parameter_added = TRUE
# - changed = TRUE
#
# Wikitext should gain:
#   | description = TEST VALUE
############################################################

result <- modify_template(
  content = old_content,
  new_value = "TEST VALUE",
  template_name = "Page data",
  param_name = "description"
)

print(result)

cat(result$content)

stopifnot(result$template_found)
stopifnot(!result$parameter_found)
stopifnot(result$parameter_added)
stopifnot(result$changed)


############################################################
# TEST 2: Replace an existing parameter
#
# Expected:
# - template_found = TRUE
# - parameter_found = TRUE
# - parameter_added = FALSE
# - changed = TRUE
#
# Original:
#   | license = CC-BY-SA-3.0
#
# New:
#   | license = TEST_LICENSE
############################################################

result <- modify_template(
  content = old_content,
  new_value = "TEST_LICENSE",
  template_name = "Page data",
  param_name = "license"
)

print(result)

cat(result$content)

stopifnot(result$template_found)
stopifnot(result$parameter_found)
stopifnot(!result$parameter_added)
stopifnot(result$changed)


############################################################
# TEST 3: Replace with identical value
#
# Expected:
# - template_found = TRUE
# - parameter_found = TRUE
# - parameter_added = FALSE
# - changed = FALSE
#
# Since the value is already:
#   | license = CC-BY-SA-3.0
#
# No modification should occur.
############################################################

result <- modify_template(
  content = old_content,
  new_value = "CC-BY-SA-3.0",
  template_name = "Page data",
  param_name = "license"
)

print(result)

cat(result$content)

stopifnot(result$template_found)
stopifnot(result$parameter_found)
stopifnot(!result$parameter_added)
stopifnot(!result$changed)