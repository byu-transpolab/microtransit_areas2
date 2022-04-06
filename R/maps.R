

#' Get the population of census tracts
#' 
#' 
get_census_pop <- function(areas){
  
  variables <- c(
    "population" = "B02001_001", # TOTAL: RACE
    "housing_units" = "B25001_001", # HOUSING UNITS
    "households" = "B19001_001" #HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
  )
  
  
  pop <- get_acs(geography = "block group", variables = variables,
          state = "UT", county = c("Salt Lake", "Davis", "Utah"), geometry = TRUE) %>%
    select(-moe) %>%
    spread(variable, estimate) %>%
    # area is in m^2, change to km^2
    mutate(area = as.numeric(st_area(geometry) * 1e-6)) %>%
    transmute(
      geoid = GEOID,
      group = 1,
      population, households,  housing_units,
      density = households / area,
    ) %>%
    filter(population > 0)  %>%
    st_transform(26912)
    
    
    areas %>%
      st_join(pop) %>%
      group_by(name) %>%
      summarise(
        across(c(population, households, housing_units), sum)
      )
}

area_loading <- function(area_pop, soslc_load = 17){
  
  index_slco <- which(area_pop$name == "South SL Co")
  
  area_pop %>%
    mutate(
      # compute area
      area = st_area(.),
      
      # compute area as ratio to south slco
      area_share = area / area[index_slco],
      area_vehicles = ceiling(area_share * soslc_load),
      
      # compute share of hh
      household_share = households / households[index_slco],
      
      # vehicles
      hh_vehicles = ceiling(soslc_load * household_share)
      
    )  %>%
    st_set_geometry(NULL)
  
}


#' Extract rail line shapes from the UTA GTFS file
#' 
#' 
#' @param gtfs_file path to the UTA gtfs, downloadable at https://gtfsfeed.rideuta.com
get_rail_lines <- function(gtfs_file){
  gtfs <- read_gtfs(gtfs_file)
  
  rail_services <- gtfs$routes %>% as_tibble() %>% filter(grepl("Line", route_long_name) | 
                                           grepl("FrontRunner", route_long_name))
  
  rail_trips <- gtfs$trips %>% filter(route_id %in% rail_services$route_id) %>%
    group_by(route_id) %>% slice(6)
  
  
  convert_shapes_to_sf(gtfs) %>% 
    filter(shape_id %in% rail_trips$shape_id) %>%
    left_join(rail_trips) %>%
    left_join(rail_services)
  
}

#' A basic leaflet map of the UTA system
#' 
#' Uses the colors encoded in the GTFS file!
rail_map <- function(rail_lines){
  
  colors <- colorFactor(palette = paste("#", lines$route_color, sep = ""), levels = lines$route_long_name)
    
  leaflet(lines) %>%
    addProviderTiles(providers$CartoDB) %>%
    addPolylines(color = ~colors(route_long_name))
  
}