library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "bookdown", "sf", "tidycensus", "ggspatial"))
options(tigris_use_cache = TRUE)

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/maps.R")
source("R/event_handler.R")


data_targets <- tar_plan(
  tar_target(area_shp, "data/areas.geojson", format = "file"),
  tar_target(areas, st_read(area_shp) %>% st_transform(26912)),
  tar_target(area_pop, get_census_pop(areas)),
  tar_target(loading, area_loading(area_pop)),
  
  ###Event handler data
  tar_taret(ex1, "Existing_1.csv", format = "file"),
  tar_taret(ex5, "Existing_5.csv", format = "file"),
  tar_taret(sp1, "Split_1.csv", format = "file"),
  tar_taret(sp5, "Split_5.csv", format = "file"),
  tar_taret(a1, "A_1.csv", format = "file"),
  tar_taret(a5, "A_5.csv", format = "file"),
  tar_taret(b1, "B_1.csv", format = "file"),
  tar_taret(b5, "B_5.csv", format = "file"),

)



