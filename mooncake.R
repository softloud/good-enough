library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)

# Read the CSV file (adjust the file path as needed)
dev_plan <- read_csv("Jira Export CSV (all fields) 20250305205856.csv")

# For this demonstration, assume each issue is a node identified by "Issue key"
# and that the column "Inward issue link (Blocks)" contains the target issue(s) for a blocking relation.
# If there are multiple links per cell, additional parsing may be required.
nodes <- dev_plan %>%
  select(Issue_key = `Issue key`, Summary) %>%
  distinct()

edges <- dev_plan %>%
  select(from = `Issue key`, to = `Inward issue link (Blocks)`) %>%
  filter(!is.na(to) & to != "")

# Construct the graph (directed)
dev_graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# Compute k-core decomposition using igraph's coreness (via tidygraph's centrality_cores)
dev_graph <- dev_graph %>%
  activate(nodes) %>%
  mutate(coreness = centrality_cores())

# Print nodes with their core values
dev_graph %>%
  as_tibble() %>%
  arrange(desc(coreness)) %>%
  print(n = Inf)

# Visualize the graph with node color reflecting its coreness value
ggraph(dev_graph, layout = "fr") +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm')), alpha = 0.5) +
  geom_node_point(aes(color = coreness), size = 5) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "K-core Decomposition of the Development Plan",
       subtitle = "Core values indicate key invariant substructures for governance")
