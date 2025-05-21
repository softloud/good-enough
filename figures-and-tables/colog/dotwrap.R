library(readODS)
library(here)
library(tidyverse)

dat <- read_ods(
  here("figures-and-tables/colog/colog_input.ods"),
  skip=1) 

colnames(dat)

dat_assumption <- dat |>
  select(
    assumption, 
    representation, represented, representor,
    representation_to_representor, representation_to_represented, represented_to_representor) 


dat_nodes <- dat |>
  select(
    node, 
    type,
    label,
    subgraph,
    subsubgraph
  )

# our objective is to tell a story

# assumption 1
# a scientist analyses some data

assumption_1_input <- dat_assumption |>
  filter(assumption==1) |>
  select(assumption, representation, represented, representor)

# Now, we need the edge labels from the representation graph.

assumption_rep <- dat_assumption |>
  filter(assumption == 0)

t(assumption_rep)

assumption_1 <-
  assumption_1 |>
    select(-assumption) |>
    pivot_longer(
      cols = everything(),
      names_to = "representational",
      values_to = "assumptional"
    ) |>
    mutate(
      assumption=assumption_1$assumption
    )

ass_1_obs <- assumption_1 |>
  select(representational, assumptional)

ass_1_edges <-
  assumption_rep |>
    select(contains("_to_")) |>
    pivot_longer(
      cols = everything(),
      names_to = "edge_relation",
      values_to = "edge_label"
    ) |>
    separate(edge_relation,
      remove = FALSE,
      into =  c("rep_source", "rep_target"),
      sep="_to_"
      ) |>
    left_join(ass_1_obs |> rename(obj_source=assumptional),
      by = c("rep_source" = "representational")) |>
    left_join(ass_1_obs |> rename(obj_target=assumptional),
      by = c("rep_target" = "representational"))


# given some data frame do some things



make_dot_df <- function(df) {
  df |>
    select(obj_source, obj_target) |>
    mutate(
      dot_line = str_c(obj_source, " -> ", obj_target)
    )

}


make_dot_df_output <- make_dot_df(ass_1_edges)

make_dot_str <- function(make_dot_df_output) {
  make_dot_df_output |>
    pull(dot_line) |>
    str_c(collapse = "\n") 
}

library(DiagrammeR)

dot_graph <- str_c("digraph colog_example {\n", make_dot_str(make_dot_df(ass_1_edges)), "\n}")


grViz(dot_graph)

