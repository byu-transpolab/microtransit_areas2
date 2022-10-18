
mmake_areas_map <- function(crs, SLCSouth, WestCity, Sandy, WestJordan){
  pal <- wesanderson::wes_palette("Moonrise2")
  colors <- c("SLCSouth" = pal[1],
              "WestCity" = pal[2],
              "Sandy" = pal[3],
              "WestJordan" = pal[4])
  ggplot() + 
    annotation_map_tile("cartolight", zoom = 12) + 
    coord_sf(crs = crs) +
    geom_sf(data = WestCity, aes(fill = "WestCity"), inherit.aes = F, alpha = 0.8) +
    geom_sf(data = Sandy, aes(fill = "Sandy"), inherit.aes = F, alpha = 0.8) +
    geom_sf(data = WestJordan, aes(fill = "WestJordan"), inherit.aes = F, alpha = 0.8) +
    geom_sf(data = SLCSouth, aes(fill = "SLCSouth"), inherit.aes = F, alpha = 0.8) +
    theme_map() +
    scale_fill_manual(name = "Zones", values = colors)
}

make_areas_map <- function(crs, areas_file){
  
  areas <- st_read(areas_file)
  
  ggplot() +
    annotation_map_tile("cartolight", zoom = 12) +
    layer_spatial(areas, aes(fill = name), alpha = 0.5) +
    theme_map()
  
}

#' 
#' #' Get the population of census tracts
#' #' 
#' #' 
#' get_census_pop <- function(areas){
#'   
#'   variables <- c(
#'     "population" = "B02001_001", # TOTAL: RACE
#'     "housing_units" = "B25001_001", # HOUSING UNITS
#'     "households" = "B19001_001" #HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
#'   )
#'   
#'   
#'   pop <- get_acs(geography = "block group", variables = variables,
#'           state = "UT", county = c("Salt Lake", "Davis", "Utah"), geometry = TRUE) %>%
#'     select(-moe) %>%
#'     spread(variable, estimate) %>%
#'     # area is in m^2, change to km^2
#'     mutate(area = as.numeric(st_area(geometry) * 1e-6)) %>%
#'     transmute(
#'       geoid = GEOID,
#'       group = 1,
#'       population, households,  housing_units,
#'       density = households / area,
#'     ) %>%
#'     filter(population > 0)  %>%
#'     st_transform(26912)
#'     
#'     
#'     areas %>%
#'       st_join(pop) %>%
#'       group_by(name) %>%
#'       summarise(
#'         across(c(population, households, housing_units), sum)
#'       )
#' }
#' 
#' area_loading <- function(area_pop){
#'   
#'   index_slco <- which(area_pop$name == "South SL Co")
#'   
#'   area_pop %>%
#'     mutate(
#'       # compute area
#'       area = st_area(.),
#'       
#'       # compute area as ratio to south slco
#'       area_share = area / area[index_slco],
#'       area_vehicles = ceiling(area_share * 12),
#'       
#'       # compute share of hh
#'       household_share = households / households[index_slco],
#'       
#'       # vehicles
#'       hh_vehicles = ceiling(12 * household_share)
#'       
#'     )  %>%
#'     st_set_geometry(NULL)
#'   
#' }