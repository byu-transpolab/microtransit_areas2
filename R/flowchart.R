graphh <- create_graph() %>%
  add_nodes_from_table("report/flowchart_nodes.csv", label_col = label) %>% 
  add_edges_from_table("report/flowchart_edges.csv", from_col = from, to_col = to, from_to_map = label) %>%
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

  export_graph(graphh, file_name = "setset.png",
               file_type = "png",
               width = 500,
               height = 500)
  render_graph(graphh)
