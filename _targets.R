library(targets)
library(tarchetypes)
library(future)
library(future.apply)
library(furrr)
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
                            "gtfstools",
                            "ragg"))
options(tigris_use_cache = TRUE)

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/maps.R")
source("R/event_handler.R")
source("R/data_helpers.R")
source("R/data_viz.R")
source("R/analysis.R")

# note: a script to copy the files from Box is in the sh/ folder


data_targets <- tar_plan(
  tar_target(area_shp, "data/areas.geojson", format = "file"),
  tar_target(areas, st_read(area_shp) %>% st_transform(26912)),
  tar_target(area_pop, get_census_pop(areas)),
  tar_target(loading, area_loading(area_pop)),
  
  ###Event handler data
  tar_target(ex1, "data/Exist_1.csv.gz", format = "file"),
  tar_target(ex5, "data/Exist_5.csv.gz", format = "file"),
  tar_target(sp1, "data/Split_1.csv.gz", format = "file"),
  tar_target(sp5, "data/Split_5.csv.gz", format = "file"),
  tar_target(a1, "data/A_1.csv.gz", format = "file"),
  tar_target(a5, "data/A_5.csv.gz", format = "file"),
  tar_target(b1, "data/B_1.csv.gz", format = "file"),
  tar_target(b5, "data/B_5.csv.gz", format = "file"),
  tar_target(c1, "data/C_1.csv.gz", format = "file"),
  tar_target(d1, "data/D_1.csv.gz", format = "file"),

  ###Scenarios list
  scenario_list = list(
    "Existing 1 it" = ex1,
    "Existing 5 it" = ex5,
    "Split 1 it" = sp1,
    "Split 5 it" = sp5,
    "A 1 it" = a1,
    "A 5 it" = a5,
    "B 1 it" = b1,
    "B 5 it" = b5,
    "C 1 it" = c1,
    "D 1 it" = d1
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
  
  b5events = read_csv(b5, col_types = coltypes),
  events_list = future_map(scenario_list, read_events, cols)
)


analysis_targets <- tar_plan(
  mode_choice_table = all_join(events_list, mode_choice, "mode", "mode"),
  num_passengers = all_join(events_list, rh_pass, "numPassengers", "num_passengers"),
  reserve_times = all_join(events_list, rh_times, "rhReserveOutcome", "Outcome"),
  utilization = all_join(events_list, rh_utilization, "area", "Area", rh_info),

  rh_to_transit = all_join(events_list, count_rh_transit_transfers, "area", "Area"),
  transit_to_rh = all_join(events_list, count_transit_rh_transfers, "area", "Area"),
  rh_transit_tot = rbind(rh_to_transit, transit_to_rh) %>%
    group_by(Area) %>%
    summarize_all(sum, na.rm = T),
  
  # disaggregated
  reserve_disag = make_reserve_times(events_list),
  utilization_disag = make_utilization(events_list),
  ridership = make_ridership(events_list),
  
)


viz_targets <- tar_plan(
  viz_reserve_times = plot_reserve_times(reserve_times),
  viz_travel_times = plot_travel_times(events_list),
  
  tar_target(gtfs_file, "data/gtfs.zip", format = "file"),
  rail_lines = get_rail_lines(gtfs_file),
  
)



#### Run all targets ####
tar_plan(
  data_targets,
  analysis_targets,
  viz_targets
)