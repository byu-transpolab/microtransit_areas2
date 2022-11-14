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