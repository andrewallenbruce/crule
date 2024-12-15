source(here::here("data-raw", "pins_internal.R"))

# -- Data wrangling code here -- #

pin_update(
  dataset,
  name = "object_name",
  title = "Short Description",
  description = "Long Description"
)
