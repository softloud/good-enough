library(R6)
library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(DiagrammeR)
library(readODS)

# === Extract Layer ===
# input arrow: raw data
# output arrow: semantically-defined extractions--must have id (todo)

ExtractLayer <- R6Class("ExtractLayer",
  public = list(
    filepath = NULL,

    initialize = function(filepath) {
      self$filepath <- filepath

      ass <- read_ods(filepath, range = "A3:K20") |>
      dplyr::filter(!is.na(representation)) |>
      mutate(
        assumption = 1:n(),
        representation_represented = str_c(representation, "_", represented),
        representation_representor = str_c(representation, "_", representor),
        represented_representor = str_c(represented, "_", representor)
      )

      private$ass_dat <- ass

      node_base <- read_ods(filepath, range = "I3:K20")

      rep_nodes <- node_base |>
        filter(node_type == "represents") |>
        crossing(assumption = private$ass_dat$assumption) |>
        mutate(node = str_c(node, "_", assumption)) |>
        select(-assumption)

      # all others get tagged with assumption 0 for now
      non_rep_nodes <- node_base |>
        filter(node_type != "represents")

      private$node_dat <- bind_rows(rep_nodes, non_rep_nodes)      
      private$dependency_dat <- read_ods(filepath, range = "L3:N20")      
      private$edge_dat <- private$build_edges()

    },

    # Public accessors
    get_nodes = function() private$node_dat,
    get_assumptions = function() private$ass_dat,
    get_dependencies = function() private$dependency_dat,
    get_edges = function() private$edge_dat,
    get_rep_edges = function() private$representational_edges
  ),

  private = list(
    ass_dat = NULL,
    node_dat = NULL,
    dependency_dat = NULL,
    edge_dat = NULL,

    build_edges = function() {

      all_edges <- 
        bind_rows(
          private$ass_dat |>
            mutate(rep_rel = "representation_represented") |>
            select(
              assumption,
              rep_rel,
              edge_rel = representation_represented,
              obj_source = representation,
              obj_target = represented,
              edge_label = representation_to_represented
            ),
          private$ass_dat |>
            mutate(rep_rel = "representation_representor") |>
            select(
              assumption,
              rep_rel,
              edge_rel = representation_representor,
              obj_source = representation,
              obj_target = representor,
              edge_label = representation_to_representor
            ),
          private$ass_dat |>
            mutate(rep_rel = "represented_representor") |>
            select(
              assumption,
              rep_rel,
              edge_rel = represented_representor,
              obj_source = represented,
              obj_target = representor,
              edge_label = represented_to_representor
            )
        )

    representational_edges <- all_edges |>
      filter(assumption == 0) |>
      select(-assumption) |>
      cross_join(
        private$ass_dat |> select(assumption)
      ) |>
      mutate(obj_source = if_else(
        str_detect(obj_source, "represent"), str_c(obj_source, "_", assumption), obj_source),
        obj_target = if_else(
        str_detect(obj_target, "represent"), str_c(obj_target, "_", assumption), obj_target)
      )

    # set representational edges, this seems out of place probably need to refactor
    private$representational_edges <- representational_edges

    # return everything
    bind_rows(all_edges |> filter(assumption != 0), representational_edges)
    }
  )
)

# === Semantic Layer ===
# input extractions
# output semantic dataframes (source aggregated)

SemanticLayer <- R6Class("SemanticLayer",
  public = list(
    nodes = data.frame(),
    edges = data.frame(),

    initialize = function(nodes, edges) {
      self$nodes <- nodes |>
        mutate(
          subgraph = if_else(
            node_type == "represents",
            "representational",
            "sis"
          ),
          subsubgraph = case_when(
            node_type == "human" ~ "human",
            node_type == "automata" ~ "automata",
            # need to append with assumption to separate representational categories
            node_type == "represents" ~  "diamond",
            TRUE ~ NA

          ),
          node_shape = case_when(
          node_type == "represents" ~ "diamond",
          node_type == "human" ~ "ellipse",
          node_type == "automata" ~ "hexagon",
          TRUE ~ "parallelogram"
        ),
        node_label = case_when(
          # must do representational first
          subgraph == "representational" ~ str_c(node),

          # we want representation names as labels for now
          (subgraph != "representational" & !is.na(node_label)) ~ node_label,

          TRUE ~ node
        ),
        node_options = str_c(
          "[label=\"", node_label, "\",",
          "shape=", node_shape,
          "]"),
        dot_line = str_c("\t", node, " ", str_replace_na(node_options, "")),
        node_id = node
        )

      # Enrich edges with edge_id and infer structure
      self$edges <- edges |>
        mutate(
          edge_id = str_c(edge_rel, "_", assumption),
          edge_label = if_else(is.na(edge_label), "", edge_label),
          edge_style = case_when(
            rep_rel == "representation_representor" ~ "dotted",
            rep_rel == "representation_represented" ~ "solid",
            rep_rel == "represented_representor" ~ "dashed"
          ),
          edge_arrow = str_c(obj_source, " -> ", obj_target),
          edge_options = str_c(
            "[label=\"", edge_label, " \", ", 
            "style=", edge_style, 
            "]"),
          dot_line = str_c("\t", edge_arrow, " ", str_replace_na(edge_options, ""))
        ) |>
        select(
          edge_id,
          edge_arrow,
          edge_options,
          dot_line
        )

      


    },

    get_nodes = function() self$nodes |> select(
      node, 
      subgraph,
      dot_line),
    get_edges = function() self$edges
  )
)


# === Output Layer ===
# in this case, the third layer is about production

DotMaker <- R6Class("DotMaker",
  public = list(
    edge_df = data.frame(),
    node_df = data.frame(),
    dot_string = character(),
    dot_graph = character(),
    
    initialize = function(nodes, edges) {

      # initialise these first
      self$node_df <- nodes
      self$edge_df <- edges 
      
      # now for some other stuff
      self$dot_string <- self$edge_df |>
        pull(dot_line) |>
        str_c(collapse = "\n")
      
      self$dot_graph <- str_c("digraph G {\n",
        self$generate_nested_subgraphs(),
        "\n",

        # is a bit hacky to tack all edges on after clusters
        self$dot_string, 
        "\n}\n")

  },

  generate_subgraph_clusters = function() {
  subgraphs <- unique(self$node_df$subgraph)

  subgraph_blocks <- purrr::map_chr(subgraphs, function(subgraph_name) {
    sub_nodes <- self$node_df %>%
      filter(subgraph == subgraph_name)

    dot_lines <- str_c("\t",sub_nodes$dot_line, collapse = "\n")

    glue("\tsubgraph cluster_{subgraph_name} {{
      {dot_lines}
      \t}}\n")
    })
  
    str_c(subgraph_blocks, collapse = "\n\n")
  },

  generate_nested_subgraphs = function() {
  subgraphs <- unique(self$node_df$subgraph)

  subgraph_blocks <- purrr::map_chr(subgraphs, function(sg) {
    sg_df <- self$node_df |> filter(subgraph == sg)
    subsubs <- unique(sg_df$subsubgraph)

    subsub_blocks <- purrr::map_chr(subsubs, function(ssg) {
      ssg_df <- sg_df |> filter(subsubgraph == ssg)
      dot_lines <- str_c(ssg_df$dot_line, collapse = "\n")
      glue("\t\tsubgraph cluster_{sg}_{ssg} {{
\t\t\tlabel = \"{ssg}\"
{dot_lines}
\t\t}}")
    })

    glue("\tsubgraph cluster_{sg} {{
\t\tlabel = \"{sg}\"
{str_c(subsub_blocks, collapse = '\n\n')}
\t}}")
  })

  str_c(subgraph_blocks, collapse = "\n\n")
},

    
    plot = function() {
      grViz(self$dot_graph)
    },
    
    print_dot = function() {
      cat(self$dot_graph)
    }
  )
)
