# Make map of study areas

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
  
  ggsave("image/areas_map.png", map, scale = 2) 
  
  map
}


# Create flowchart for pipeline

create_pipeline_flowchart <- function(outfile, outtype, width, height) {
  
 flowchart <- create_graph() %>%
    add_nodes_from_table("data/flowchart_nodes.csv", label_col = label) %>% 
    add_edges_from_table("data/flowchart_edges.csv", from_col = from, to_col = to, from_to_map = label) %>%
    add_global_graph_attrs(
      c("layout", "ordering"),
      c("dot", "in"),
      "graph") %>%
    add_global_graph_attrs(
      c("fixedsize", "color", "fillcolor", "fontcolor"),
      c("false", "black", "powderblue", "black"),
      "node") %>%
    add_global_graph_attrs(
      c("color"),
      c("black"),
      "edge") %>% 
    set_node_attrs(fontcolor, "black") %>%
    set_node_attr_to_display("text")
 
 export_graph(flowchart, file_name = outfile,
               file_type = outtype,
               width = width,
               height = height)
 
 render_graph(flowchart)
 
}


# Create graph for beam calibration

graph_beam_calibration <- function(share, target){
  
  targets <- read_csv(target) %>% 
    mutate(mode = case_when(
      mode %in% c("car", "hov2", "hov3") ~ "Car",
      str_detect(mode, "ride_hail") ~ "Ridehail",
      str_detect(mode, "transit") ~ "Transit",
      TRUE ~ str_to_title(mode)
    )) %>% 
    group_by(mode) %>% 
    summarise(trips = sum(tripTotals)) %>% 
    mutate(pct = trips / sum(trips))
  
  shares <- read_csv(share) %>% 
    mutate(mode = case_when(
      mode %in% c("car", "hov2", "hov3") ~ "Car",
      str_detect(mode, "ride_hail") ~ "Ridehail",
      str_detect(mode, "transit") ~ "Transit",
      TRUE ~ str_to_title(mode)
    )) %>% 
    group_by(iteration, mode) %>% 
    summarise(trips = sum(tripTotals)) %>% 
    mutate(pct = trips / sum(trips))
  
  beam_calib_plot <- shares %>% 
    ggplot() +
    geom_line(aes(x = iteration, y = pct, color = mode)) +
    geom_hline(
      data = targets,
      aes(yintercept = pct, color = mode),
      linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    scale_x_continuous(
      breaks = 1:max(shares$iteration),
      limits = c(1,max(shares$iteration)),
      minor_breaks = NULL,
      expand = c(0,0)) +
    theme_bw() +
    labs(
      x = "Calibration Iteration",
      y = "Mode Share",
      color = "Mode")
  
  ggsave(
    "image/beam_calibration_graph.png",
    beam_calib_plot,
    width = 7,
    height = 4,
    units = "in")
  
  beam_calib_plot
  
}



# Create table for beam calibration

table_beam_calibration <- function(share, target){
  
  targets <- read_csv(target) %>% 
    mutate(mode = case_when(
      mode %in% c("car", "hov2", "hov3") ~ "Car",
      str_detect(mode, "ride_hail") ~ "Ridehail",
      str_detect(mode, "transit") ~ "Transit",
      TRUE ~ str_to_title(mode)
    )) %>% 
    group_by(mode) %>% 
    summarise(target_trips = sum(tripTotals)) %>% 
    mutate(target_pct = target_trips / sum(target_trips))
  
  shares <- read_csv(share) %>% 
    mutate(mode = case_when(
      mode %in% c("car", "hov2", "hov3") ~ "Car",
      str_detect(mode, "ride_hail") ~ "Ridehail",
      str_detect(mode, "transit") ~ "Transit",
      TRUE ~ str_to_title(mode)
    )) %>% 
    filter(iteration == max(iteration)) %>% 
    group_by(mode) %>% 
    summarise(act_trips = sum(tripTotals)) %>% 
    mutate(actual_pct = act_trips / sum(act_trips))
  
  calibration <- targets %>% 
    left_join(shares, by = "mode") %>% 
    select(mode, target_pct, actual_pct) %>% 
    mutate(err = (actual_pct - target_pct) / target_pct) %>% 
    `names<-`(c("Mode", "Target Mode Share", "Actual Mode Share", "% Error"))
  
  calibration
  
}


# Write tables to csv

write_tables <- function(tables){
  
  for(i in 1:length(tables)){
    
    tables[[i]] %>% 
      mutate(across(
        .cols = where(is.numeric),
        .fns = signif,
        digits = 4
      )) %>% 
    write_csv(paste0("tables/", names(tables)[[i]], ".csv"))
    
  }
  
}


# Graph mode choice convergence at iteration 11

graph_mc_converge <- function(mc){
  
  mc2 <- mc %>% 
    pivot_longer(-iterations, names_to = "mode", values_to = "trips") %>% 
    mutate(mode = case_when(
      str_detect(mode, "hov") ~ "Car",
      str_detect(mode, "ride_hail") ~ "Ridehail",
      str_detect(mode, "transit") ~ "Transit",
      TRUE ~ str_to_title(mode))) %>% 
    group_by(iterations, mode) %>% 
    summarise(tot_trips = sum(trips)) %>% 
    mutate(mode_share = tot_trips/sum(tot_trips))
  
  graph <- mc2 %>% 
    ggplot(aes(x = iterations, y = mode_share, color = mode)) +
    geom_line() +
    geom_vline(aes(xintercept = 11)) +
    theme_bw() +
    labs(x = "Iteration",
         y = "Mode Share",
         color = "Mode")
  
  ggsave(
    "image/mode_choice_convergence.png",
    graph,
    width = 6.5,
    height = 4,
    units = "in"
  )
  
  graph
  
}
