# sim_nodes <- create_node_df(
#   3,
#   type = "sim",
#   # label = c("ps", "as", "bm"),
#   label = c("PopulationSim", "ActivitySim", "BEAM"),
#   source = c(rep("Lant", 2), "Atchley"),
#   shape = 'oval',
#   fixedsize = FALSE
# )
# 
# 
# file_nodes <- create_node_df(
#   12,
#   type = "file",
#   label = c("Census \n Data", "Synthetic \n Population", "Choice \n Coefficients",
#             "Zonal \n SE Data", "Travel \n Skims", "Projected \n Plans",
#             "Choice \n Coefficients", "Network", "ODT \n Fleet", "Transit \n (GTFS) Data",
#             "Configuration", "Output \n Plans/Events"),
#   # label = c("cens", "synth", "choicea", "se", "skims", "aplans", "choiceb",
#             # "net", "fleet", "gtfs", "config", "outplans"),
#   source = c(rep("Lant", 6), rep("Atchley", 6)),
#   shape = 'rectangle',
#   fixedsize = FALSE
# )
# 
# 
# nodes <- combine_ndfs(
#   sim_nodes,
#   file_nodes
# ) %>% 
#   mutate(group = ifelse(
#     id %in% c(1, 2, 3, 4, 5, 9),
#     "center",
#     NA
#   ))
# 
# 
# 
# 
# 
# links <- create_edge_df(
#   from = 1:14,
#   to = c(5, 9, 15, 1, 2,
#          2, 2, 2, 3, 3,
#          3, 3, 3, 3),
#   rel = c(rep("out", 3), rep("in", 12))
# )
# 
# create_graph(nodes, links, attr_theme = 'tb') %>% 
#   render_graph()
