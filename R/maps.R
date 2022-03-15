make_map <- function(areas){
  
  pal <- wes_palette("Zissou1", 6, type = "continuous")
  
  ggplot(areas, aes(fill = name)) + 
    annotation_map_tile("cartolight", zoom = 12) +
    geom_sf(alpha = 0.5) +
    scale_fill_manual("Area Name", values = pal) + 
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.text=element_text(size=8), #change font size of legend text
    legend.title=element_text(size=8)
  )
}

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

area_loading <- function(area_pop){
  
  index_slco <- which(area_pop$name == "South SL Co")
  
  area_pop %>%
    mutate(
      # compute area
      area = st_area(.),
      
      # compute area as ratio to south slco
      area_share = area / area[index_slco],
      area_vehicles = ceiling(area_share * 12),
      
      # compute share of hh
      household_share = households / households[index_slco],
      
      # vehicles
      hh_vehicles = ceiling(12 * household_share)
      
    )  %>%
    st_set_geometry(NULL)
  
}