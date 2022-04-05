library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse",
                            "bookdown",
                            "sf",
                            "tidycensus",
                            "ggspatial",
                            "data.table",
                            "ragg"))
options(tigris_use_cache = TRUE)

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/maps.R")
source("R/event_handler.R")
source("R/data_helpers.R")


data_targets <- tar_plan(
  # tar_target(area_shp, "data/areas.geojson", format = "file"),
  # tar_target(areas, st_read(area_shp) %>% st_transform(26912)),
  # tar_target(area_pop, get_census_pop(areas)),
  # tar_target(loading, area_loading(area_pop)),
  
  ###Event handler data
  tar_target(ex1, "data/Existing_1.csv", format = "file"),
  tar_target(ex5, "data/Existing_5.csv", format = "file"),
  tar_target(sp1, "data/Split_1.csv", format = "file"),
  tar_target(sp5, "data/Split_5.csv", format = "file"),
  tar_target(a1, "data/A_1.csv", format = "file"),
  tar_target(a5, "data/A_5.csv", format = "file"),
  tar_target(b1, "data/B_1.csv", format = "file"),
  tar_target(b5, "data/B_5.csv", format = "file"),
  
  ###Scenarios list
  scenario_list = list("Existing 1%" = ex1,
                       "Existing 5%" = ex5,
                       "Split 1%" = sp1,
                       "Split 5%" = sp5,
                       "A 1%" = a1,
                       "A 5%" = a5,
                       "B 1%" = b1,
                       "B 5%" = b5
  ),
  
  ###Important columns
  cols = c("person",
            "vehicle",
            "time",
            "type",
            "mode",
            "legMode",
            "vehicleType",
            "vehicle",
            "arrivalTime",
            "departureTime",
            "departTime",
            "length",
            "numPassengers",
            "actType",
            "personalVehicleAvailable"
  ),
  
  
  tar_target(event_cols, "data/event_cols.csv", format = "file"),
  coltypes = get_coltypes(event_cols),
  tar_target(rh_info_path, "data/rh_info.csv", format = "file"),
  rh_info = format_rh_info(rh_info_path),
  
  
  events_list = map(scenario_list, read_events, cols)
)


analysis_targets <- tar_plan(
  mode_choice_table = all_join(events_list, mode_choice, "mode", "mode"),
  num_passengers = all_join(events_list, rh_pass, "numPassengers", "num_passengers"),
  reserve_times = all_join(events_list, rh_times, "rhReserveOutcome", "Outcome"),
  utilization = all_join(events_list, rh_utilization, "area", "Area", rh_info)
)


viz_targets <- tar_plan(
  viz_reserve_times = plot_reserve_times(reserve_times),
  viz_travel_times = plot_travel_times(events_list)
)



#### Run all targets ####
tar_plan(
  data_targets,
  analysis_targets,
  viz_targets
)


