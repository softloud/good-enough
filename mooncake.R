# Load necessary libraries
library(tidyverse)
library(tidygraph)
library(ggraph)

# Define the nodes of our categorical framework
nodes <- tibble::tibble(
  name = c(
    "Human Intent",         # The driving purpose of development
    "Machine Processing",   # Heuristic intelligence in action
    "Governance",           # The oversight mechanism that aligns intent with action
    "Ethical Alignment",    # A measure from Iris's framework: ethical coherence
    "Bias Monitoring",      # Capturing and mitigating power dynamics
    "System Robustness",    # Resilience of the system under governance
    "Trustworthiness",      # Transparency and reliability of outcomes
    "Iris Framework"        # Critical lenses for measurement and evaluation
  )
)

# Define the edges representing relationships (morphisms)
edges <- tibble::tibble(
  from = c(
    "Human Intent", "Human Intent", "Machine Processing",
    "Governance", "Governance", "Iris Framework", "Iris Framework"
  ),
  to   = c(
    "Governance", "Ethical Alignment", "Governance",
    "Bias Monitoring", "System Robustness", "Trustworthiness", "Ethical Alignment"
  ),
  label = c(
    "maps to", "informs", "constrains",
    "evaluates", "ensures", "measures", "enhances"
  )
)

# Create the directed graph
graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# Plot using a force-directed layout (Fruchterman-Reingold)
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(label = label),
                 arrow = arrow(length = unit(4, 'mm')),
                 end_cap = circle(3, 'mm'),
                 label_colour = "darkblue",
                 angle_calc = "along",
                 label_dodge = unit(2, 'mm')) +
  geom_node_point(size = 5, color = "blue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  theme_minimal() +
  ggtitle("Updated Categorical Framework Incorporating Iris' Methodology")
