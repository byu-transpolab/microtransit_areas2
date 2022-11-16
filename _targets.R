library(targets)
library(tarchetypes)

# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

library(readr)
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
  "R/prelim_comparison.R",
  "R/maps.R",
  "R/misc_viz.R"
)
purrr::map(r_files, source)

######## List targets ##########################################################

data_targets <- tar_plan(
  
  tar_target(EX, "data/wfrc_existing_events.csv.gz", format = "file"),
  tar_target(Split, "data/wfrc_split_events.csv.gz", format = "file"),
  tar_target(A, "data/wfrc_A_events.csv.gz", format = "file"),
  tar_target(B, "data/wfrc_B_events.csv.gz", format = "file"),
  tar_target(C, "data/wfrc_C_events.csv.gz", format = "file"),
  tar_target(D, "data/wfrc_D_events.csv.gz", format = "file"),
  
  tar_target(EX_fleet, "data/rh_fleets/rhFleet_Existing.csv", format = "file"),
  tar_target(Split_fleet, "data/rh_fleets/rhFleet_Split.csv", format = "file"),
  tar_target(A_fleet, "data/rh_fleets/rhFleet_A.csv", format = "file"),
  tar_target(B_fleet, "data/rh_fleets/rhFleet_B.csv", format = "file"),
  tar_target(C_fleet, "data/rh_fleets/rhFleet_C.csv", format = "file"),
  tar_target(D_fleet, "data/rh_fleets/rhFleet_D.csv", format = "file"),
  
  tar_target(Pilot_file, "data/wfrc_pilot_events.csv.gz", format = "file"),
  pilot = data.table::fread(file = Pilot_file, select = event_cols),
  tar_target(Pilot_fleet_file, "data/rh_fleets/rhFleet_Pilot.csv", format = "file"),
  pilot_fleet = read_ridehail_fleet(Pilot_fleet_file),
  
  scenarios = list(
    Existing = data.table::fread(file = EX, select = event_cols),
    Split = data.table::fread(file = Split, select = event_cols),
    "EX + Davis" = data.table::fread(file = A, select = event_cols),
    "EX + Lehi" = data.table::fread(file = B, select = event_cols),
    "EX + Sandy" = data.table::fread(file = C, select = event_cols),
    All = data.table::fread(file = D, select = event_cols)
  ),
  
  fleets = list(
    Existing = read_ridehail_fleet(EX_fleet),
    Split = read_ridehail_fleet(Split_fleet),
    "EX + Davis" = read_ridehail_fleet(A_fleet),
    "EX + Lehi" = read_ridehail_fleet(B_fleet),
    "EX + Sandy" = read_ridehail_fleet(C_fleet),
    All = read_ridehail_fleet(D_fleet)
  ),
  
  fleet_sizes = get_fleet_sizes(fleets),
  
  tar_target(scenario_key_file, "data/scenario_key.csv", format = "file"),
  scenario_key = read_csv(scenario_key_file),
  
  tar_target(zone_info_file, "data/zone_info.csv", format = "file"),
  zone_info = get_zone_info(zone_info_file),
  
  tar_target(areas_file, "data/areas.geojson", format = "file"),
  
  
  tar_target(persons_file, "data/common/persons.csv.gz", format = "file"),
  persons = read_csv(persons_file),
  
  
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
  
  
  #### Flowchart ###########################
  
  tar_target(fc_nodes, "data/flowchart_nodes.csv", format = "file"),
  tar_target(fc_edges, "data/flowchart_edges.csv", format = "file"),
  
  
  #### BEAM Calibration ####################
  
  tar_target(bm_targets, "data/beamtriptargets.csv", format = "file"),
  tar_target(bm_calib_shares, "data/calibration_shares.csv", format = "file")
  
)



analysis_targets <- tar_plan(
  
  #### UTA pilot info ########################
  
  UTA_table = make_uta_table(UTAOD, good_months),
  
  
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
  
  # ridehail_to_transit = c(number, percent, am, pm),
  
  rh_fulfillment = purrr::map(
    scenarios,
    get_rh_fulfillment
  ),
  
  user_incomes = purrr::map(
    scenarios,
    get_rh_incomes,
    persons
  )
  
)

pilot_targets <- tar_plan(
  
  pilot_ridership = get_tot_rh_passengers(pilot),
  pilot_utilization = get_rh_utilization(pilot_ridership, pilot_fleet),
  pilot_wait_times = get_avg_rh_wait_time(pilot)
    
)


viz_targets <- tar_plan(
  
  areas_map = make_areas_map(areas_file),
  
  flowchart = create_pipeline_flowchart("image/flowchart.png", "png", 900, 900),
  
  beam_calib_graph = graph_beam_calibration(bm_calib_shares, bm_targets),
  beam_calib_table = table_beam_calibration(bm_calib_shares, bm_targets),
  
  existing_comparison = compare_existing(
    UTA,
    pilot_ridership,
    pilot_utilization,
    pilot_wait_times),
  
  scenario_comparison = compare_scenarios(
    total_riders,
    utilization,
    fleet_sizes,
    average_wait_times,
    income_comparison
  ),
  
  income_comparison = compare_incomes(user_incomes),
  
  rh_fulfillment_comparison = compare_rh_fulfillment(rh_fulfillment),
  
  model_riders_per_vehicle = model_ridership(scenario_comparison),
  
  wait_time_list = list_wait_times(
    average_wait_times)
  
)

########### Run all targets ####################################################

tar_plan(
  data_targets,
  analysis_targets,
  pilot_targets,
  viz_targets
)
