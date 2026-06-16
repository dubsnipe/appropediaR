# Validate page save workflow

rm(list = ls())
devtools::load_all()

update_template_parameter(
  page_name = "User:Emilio/sandbox2", # Verify the template exists on the page
  template_name = "Page data",
  param_name = "description",
  new_value = paste(
    "Package test",
    Sys.time()
  ),
  session = session
)