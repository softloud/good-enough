library(tidyverse)
library(tidygraph)
library(ggraph)
library(gt)

source("figures-and-tables/anal-beads-of-data-dev/anal-bead-theme.R")

edges <- tribble(
  ~from, ~to,
  "source", "fair data entity", 
  "fair data entity", "fair data product", 
  "analyse", "interpret",
  "interpret", "validate",
  "validate", "document"
)

# validation 1
# validation 2

 
# ggsave("figures-and-tables/anal-beads-of-data-dev/anti-qso-dag.png")

detailed_edges <- tribble(
  ~from, ~to,
  "question", "project planning development",
  "project planning development", "scope fair data entity",
  "scope fair data entity", "ingest",
  "scope fair data entity", "deploy documentation",
  "scope fair data entity", "deploy analysis tool",
  "scope fair data entity", "document data entity",
  "ingest", "transform data entity",
  "transform data entity", "validate data entity",
  "document data entity", "validate data entity",
  "document validation of data entity", "fair data entity",
  "deploy documentation", "validate data entity",
  "validate data entity", "debug data entity",
  "debug data entity", "fair data entity",
  "scope fair data product", "transform",
  "transform", "fair data product",
  "unscoped platform engineering", "fair data product",
  "fair data product", "interpret",
  "analyse", "interpret",
  "interpret", "decision",
  "deploy analysis tool", "validate data entity",
  "scope fair data product", "unscoped platform engineering",
  "scope fair data product", "unscoped platform engineering development",
  "unscoped platform engineering development", "fair data product",
  "scope fair data product", "unscoped data engineering development",
  "unscoped data engineering development", "fair data product",
  "validate data entity", "document validation of data entity",
  "project reporting development", "scope fair data product",
  "fair data entity", "project reporting development",
  "unscoped data engineering", "fair data product",
  "scope fair data product", "unscoped data engineering",
  "scope fair data product", "analyse"
)  %>% 
  as_tbl_graph()  %>%
  activate(nodes)  %>%
  mutate(
  
  # set colour coding
  role = case_when(
    name %in% c(
      "scope fair data entity",
      "scope fair data product",
      "project planning development",
      "project reporting development"
      ) ~ "project plan documenter",
    name %in% c(
      "ingest",
      "transform data entity",
      "document data entity",
      "transform",
      "unscoped data engineering",
      "unscoped data engineering development",
      "debug data entity"
    ) ~ "data engineer",
    name %in% c(
      "deploy documentation",
      "unscoped platform engineering",
      "unscoped platform engineering development",
      "deploy analysis tool"
    ) ~ "platform engineer",
    name %in% c(
      "analyse",
      "validate data entity",
      "document validation of data entity",
      "interpret"
    ) ~ "analyst",
    name %in% c(
      "fair data entity",
      "fair data product"
    ) ~ "team goal",
    name %in% c(
      "decision",
      "question"
    ) ~ "decisionmaker"
  ) %>% 
      factor(levels = c("decisionmaker", "team goal", "project plan documenter", "platform engineer", "data engineer", "analyst")),
  
  # format labels to wrap
  name = str_wrap(name, 10)
  )   


version_0_dev <- 
  detailed_edges  %>% 
    ggraph(
    ) +
    labs(
      title = "fair data entity: version 0 of a data product"  %>% 
        str_wrap(30),
      subtitle = "living analysis development lifecycle detail up to fair data entity"  %>% 
        str_wrap(80),
      caption = "team aims for living fair data entity validation performed by analyst --> prevents crazy in the coconut analytics --> fair data product scoped --> living data analysis scoped"  %>% 
        str_wrap(50)
    ) +
    geom_edge_diagonal(
      aes(
        start_cap = label_rect(node1.name), 
        end_cap = label_rect(node2.name)
      ),
      arrow = arrow(length = unit(4, 'mm')),
      colour = "darkblue",
      alpha = 0.3
    ) +
    coord_flip() +
    scale_y_reverse() +  # Add padding to the y-axis
    anal_bead_theme(detailed_edges)

ggsave(
  "slidedeck/detailed-anti-qso-dag.png", 
  plot = version_0_dev,
  width = 30, 
  height = 20, units = "in")


ggsave(
  "ps-and-qs-manuscript/care-bear-data.png", 
  plot = version_0_dev,
  width = 12, 
  height = 20, units = "in")
