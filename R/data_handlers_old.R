#### Functions for reading and handling data ###################################

#' Read in events.csv(.gz) files of specific iterations in a given scenario
#'
#' @param scenario_dir Directory of scenario outputs (parent directory of `ITERS`)
#' @param event_cols The names of columns to read in
#' @param iters Iteration numbers whose events to read (vector or single integer)
#' 
#' @return A list of events tibbles for the scenario
#' 
#' @export
#'
read_iteration_events <- function(scenario_dir, event_cols, iters, ...){
 
  scenario <- list()
  
  for (i in iters){
    
    scenario[[as.character(i)]] <- data.table::fread(
      file = paste0(scenario_dir, "/", i, ".events.csv.gz"),
      select = event_cols
    )
  }
  
  scenario
}



#' Download data if not already present locally
#' 
#' @param get_from URI or similar to download from
#' @param save_to File to save locally
#' 
#' @return nothing 
#' 
#' @export
#' 
get_if_needed <- function(get_from, save_to){
  if (!file.exists(save_to)){
    download.file(get_from, save_to)
    archive::archive_extract(save_to)
  }
}



#' Get and process ridehail fleet
#' 
#' @param fleet_file RH fleet file
#' 
#' @return RH fleet with extra info
#' 
#' @export
#' 
read_ridehail_fleet <- function(fleet_file){
  
  rh_fleet <- list()
  
  rh_fleet[["fleet"]] <- read_csv(fleet_file) %>% 
    separate(shifts, c(NA, "startTime", "endTime", NA), remove = FALSE) %>% 
    mutate(startTime = as.integer(startTime),
           endTime = as.integer(endTime),
           shiftHours = (endTime - startTime) / 3600)
  
  rh_fleet[["fleet_hours"]] <- rh_fleet[["fleet"]] %>% 
    group_by(fleetId) %>% 
    summarise(vehicleHours = sum(shiftHours))
  
  rh_fleet
}