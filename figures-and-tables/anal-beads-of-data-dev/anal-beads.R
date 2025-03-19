library(tidyverse)
library(tidygraph)
library(ggraph)

source("figures-and-tables/anal-beads-of-data-dev/anal-bead-theme.R")

anal_beads_df <- tribble(
  ~from, ~to,
  "question", "source",
  "source", "ingest",
  "ingest", "transform",
  "transform", "validate",
  "validate", "document",
  "document", "analyse",
  "analyse", "interpret",
  "interpret", "decision", 
)  

anal_beads <- anal_beads_df %>% 
    as_tbl_graph(from = from, to = to, label = dev_lab)  %>%
  activate(nodes)  %>%
  mutate(
    # role refers to role required from from column
    role =
      case_when(
        name %in% c("source") ~ "project plan documentation",
        name %in% c("analyse", "interpret") ~ "analyst",
        name %in% c("decision", "question") ~ "decisionmaker",
        TRUE  ~ "data engineer"
      )  %>% 
      factor(levels = c("decisionmaker", "project plan documentation", "data engineer", "analyst"))
  )

anal_beads_vis <- anal_beads %>% 
  ggraph(layout = "linear") +
  labs(
    title = "expectation", 
    subtitle = "living analysis development lifecycle",
    caption = "time -->"
    ) +  
    geom_edge_link(
    aes(start_cap = label_rect(node1.name), 
    end_cap = label_rect(node2.name)), 
    colour = "darkblue",
    arrow = arrow(length = unit(4, 'mm')),
    alpha = 0.3
    ) +
    anal_bead_theme(anal_beads)

