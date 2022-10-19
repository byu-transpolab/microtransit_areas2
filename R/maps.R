make_areas_map <- function(areas_file){
  
  areas <- st_read(areas_file)
  
  map <- ggplot() +
    annotation_map_tile("cartolight", zoom = 11, progress = "none") +
    layer_spatial(areas, aes(fill = name), alpha = 0.5) +
    coord_sf(
      xlim = c(-112.2, -111.7), ylim = c(40.35, 40.96), crs = 4326) +
    annotation_scale(
      width_hint = 0.5, unit_category = "imperial", location = "bl") +
    annotation_north_arrow(
      style = north_arrow_fancy_orienteering,
      location = "bl",
      pad_y = unit(0.5, "cm")) +
    theme_map() +
    theme(
      legend.justification = c(0,1),
      legend.position = c(0.03,.98)
    ) +
    labs(fill = "Area")
 
  ggsave("report/image/areas_map.png", map, scale = 2) 
  
  map
}