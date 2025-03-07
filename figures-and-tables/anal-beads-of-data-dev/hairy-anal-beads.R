library(tidyverse)
library(tidygraph)
library(ggraph)

source("figures-and-tables/anal-beads-of-data-dev/anal-bead-theme.R")
source("figures-and-tables/anal-beads-of-data-dev/anal-beads.R")

hairy_anal_beads <- tribble(
  ~from, ~to, ~reason,
  "interpret", "analyse", "measure misunderstood",
  "interpret", "transform", "measure misunderstood",
  "analyse", "ingest", "not updating",
  "decision", "interpret", "analysis does not inform decision",
  "analyse", "transform", "needs different data shape",
  "analyse", "interpret", "no time for documentation"
)   %>% 
  mutate(
    reason = str_wrap(reason, 15)
  )  %>% 
  bind_rows(anal_beads_df)  %>% 
    mutate(reason_na = is.na(reason),
      project_integrity = if_else(
        is.na(reason),
        "backlog",
        "actioned"
      ),
      reason = if_else(is.na(reason), "", reason)
      )  %>% 
  as_tbl_graph() %>%
  activate(nodes)  %>%
  mutate(
    # role refers to role required from from column
    role =
      case_when(
        name %in% c("source") ~ "project plan documentation",
        name %in% c("analyse", "interpret") ~ "analyst",
        name %in% c("question", "decision") ~ "decisionmaker",
        TRUE  ~ "data engineering"
      )  %>% 
      factor(levels = c("decisionmaker", "project plan documentation", "data engineering", "analyst"))
  )

hairy_anal_beads_vis <-
  hairy_anal_beads %>% 
    ggraph(layout = "linear") +
    labs(
      title = "reality",
      subtitle = "living analysis development lifecycle",
      caption = "<-- time has no meaning -->"
    ) +
    geom_edge_arc(
      aes(
        start_cap = label_rect(node1.name), 
        end_cap = label_rect(node2.name),
        linetype = project_integrity,
        label = reason
      ),
      label_colour = "darkblue",
      label_alpha = 0.7,
      vjust = 0.5,
      colour = "darkblue",
      alpha = 0.3,
      arrow = arrow(length = unit(4, 'mm'))
    ) + 
    anal_bead_theme(hairy_anal_beads)

ggsave("slidedeck/hairy-anal-beads.png",
  plot = hairy_anal_beads_vis, width = 20, height = 9, units = "in")
ggsave("ps-and-qs-manuscript/hairy-anal-beads.png",
  plot = hairy_anal_beads_vis, width = 15, height = 10, units = "in")
