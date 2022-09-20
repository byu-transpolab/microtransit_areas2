library(targets)
library(tarchetypes)

# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

library(readr)
library(purrr)
library(qs)

# Read package list from file
package_list <- readLines("package_list.txt")

# Set target-specific options such as packages.
tar_option_set(
  packages = package_list,
  garbage_collection = TRUE,
  format = "qs")


# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
r_files <- c(
  "R/data_handlers.R",
  "R/summarize_events.R",
  "R/UTAOD_comparison.R",
  "R/prelim_comparison.R"
)
purrr::map(r_files, source)

######## List targets ##########################################################

data_targets <- tar_plan(
  
  tar_target(EX, "data/wfrc_existing_events.csv.gz", format = "file"),
  tar_target(A, "data/wfrc_A_events.csv.gz", format = "file"),
  tar_target(B, "data/wfrc_B_events.csv.gz", format = "file"),
  tar_target(C, "data/wfrc_C_events.csv.gz", format = "file"),
  tar_target(D, "data/wfrc_D_events.csv.gz", format = "file"),
  
  tar_target(EX_fleet, "data/rh_fleets/rhFleet_Existing.csv", format = "file"),
  tar_target(A_fleet, "data/rh_fleets/rhFleet_A.csv", format = "file"),
  tar_target(B_fleet, "data/rh_fleets/rhFleet_B.csv", format = "file"),
  tar_target(C_fleet, "data/rh_fleets/rhFleet_C.csv", format = "file"),
  tar_target(D_fleet, "data/rh_fleets/rhFleet_D.csv", format = "file"),
  
  scenarios = list(
    existing = data.table::fread(file = EX, select = event_cols),
    A = data.table::fread(file = A, select = event_cols),
    B = data.table::fread(file = B, select = event_cols),
    C = data.table::fread(file = C, select = event_cols),
    D = data.table::fread(file = D, select = event_cols)
  ),
  
  fleets = list(
    existing = read_ridehail_fleet(EX_fleet),
    A = read_ridehail_fleet(A_fleet),
    B = read_ridehail_fleet(B_fleet),
    C = read_ridehail_fleet(C_fleet),
    D = read_ridehail_fleet(D_fleet)
  ),
  
  fleet_sizes = get_fleet_sizes(fleets),
  
  
  #Names and types of cols to keep for events files
  tar_target(event_cols_file, "data/eventCols.csv", format = "file"),
  event_cols = get_event_cols(event_cols_file),

  
  #### UTA On Demand ##########################
  
  #Get UTA On Demand pilot program info
  tar_target(UTAOD, "data/UTAODpilotinfo.csv", format = "file"),
  
  #months for which the observed data is good
  good_months = c("JAN", "FEB", "MAR"),
  
  UTA = readr::read_csv(UTAOD) %>%
    filter(Month %in% good_months) %>%
    pivot_uta(),
  
)



analysis_targets <- tar_plan(
  
  #### Ridehail events #######################
  
  ridehail_modes = c("ride_hail",
                     "ride_hail_pooled",
                     "ride_hail_transit"),
  
  total_riders = purrr::map(
    scenarios,
    get_tot_rh_passengers),
  
  rh_trips = purrr::map(
    scenarios,
    get_ridehail_trips,
    ridehail_modes),
  
  utilization = purrr::map2(
    total_riders, fleets,
    get_rh_utilization),
  
  average_wait_times = purrr::map(
    scenarios,
    get_avg_rh_wait_time),
  
  # ridehail_to_transit = c(number, percent, am, pm)
  
)



viz_targets <- tar_plan(
  
  existing_comparison = compare_existing(
    UTA, total_riders$existing,
    utilization$existing,
    average_wait_times$existing),
  
  ridership_comparison = compare_riders(
    total_riders),
  
  utilization_comparison = compare_utilization(
    utilization,
    fleet_sizes),
  
  wait_time_comparison = compare_wait_times(
    average_wait_times),
  
  
  # Combine all comparisons for easy loading/viewing
  all_comparisons = list(
    "Existing comparison" = existing_comparison,
    "Ridership comparison" = ridership_comparison,
    "Utilization comparison" = utilization_comparison,
    "Wait time comparison" = wait_time_comparison
  )
  
)



render_targets <- tar_plan(
    # report = bookdown::render_book(
    #   input = ".", output_yaml = "_output.yml", config_file = "_bookdown.yml")
)

########### Run all targets ####################################################

tar_plan(
  data_targets,
  analysis_targets,
  viz_targets,
  render_targets
)
