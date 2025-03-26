# Set edge design
# Edges

# Core expected living analysis lifecycle workflow
anal_beads_df <- tibble::tribble(
  ~from, ~to,
  "question", "source",
  "source", "ingest",
  "ingest", "transform",
  "transform", "validate",
  "validate", "document",
  "document", "analyse",
  "analyse", "interpret",
  "interpret", "decision", 
) |>
  dplyr::mutate(
    line_type = "intended"
  )

detailed_edges <- tibble::tribble(
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
) |>
  dplyr::mutate(line_type = "intended")

detailed_nodes <- tibble::tibble(
  node = c(detailed_edges$from, detailed_edges$to)
) |>
  dplyr::distinct() |>
  dplyr::mutate( 
  # set colour coding
  node_colour = dplyr::case_when(
    node %in% c(
      "scope fair data entity",
      "scope fair data product",
      "project planning development",
      "project reporting development"
      ) ~ "project planning",
    node %in% c(
      "ingest",
      "transform data entity",
      "document data entity",
      "transform",
      "unscoped data engineering",
      "unscoped data engineering development",
      "debug data entity"
    ) ~ "data engineering",
    node %in% c(
      "deploy documentation",
      "unscoped platform engineering",
      "unscoped platform engineering development",
      "deploy analysis tool"
    ) ~ "platform engineering",
    node %in% c(
      "analyse",
      "validate data entity",
      "document validation of data entity",
      "interpret"
    ) ~ "data analysis",
    node %in% c(
      "fair data entity",
      "fair data product"
    ) ~ "project planning",
    node %in% c(
      "decision",
      "question"
    ) ~ "decision making"
  )
  )

# I expect there to be an edge from transform to analyse
# and transform to be labelled as analytics engineering
# and analyse to be labelled as data analytics

# Edges methods
ButtonEdgeDesignCategory <- setRefClass(
  "ButtonEdgeDesignCategory",
  fields = list(
    expected_edge_cols = "character",
    edges = "data.frame",

    preset = "character"
  ),
  methods = list(
    initialize = function(preset = "anal_beads") {
      callSuper(preset = preset)
      setEdges(preset)
    },
    setEdges = function(preset = "anal_beads") {
      expected_edge_cols <<- c("from", "to", "line_type")

      if (preset == "anal_beads") {
        edges <<- anal_beads_df
      } else if (preset == "hairy_anal_beads") {
        edges <<- tibble::tribble(
          ~from, ~to, ~reason,
          "interpret", "analyse", "measure misunderstood",
          "interpret", "transform", "measure misunderstood",
          "analyse", "ingest", "not updating",
          "decision", "interpret", "analysis does not inform decision",
          "analyse", "transform", "needs different data shape",
          "analyse", "interpret", "no time for documentation"
        ) |> 
          dplyr::mutate(
            reason = stringr::str_wrap(reason, 15),
            line_type = "unintended"
          )  |> 
          dplyr::bind_rows(anal_beads_df)  |> 
            dplyr::mutate(reason_na = is.na(reason),
              project_integrity = dplyr::if_else(
                is.na(reason),
                "backlog",
                "actioned"
              ),
              reason = dplyr::if_else(is.na(reason), "", reason)
              ) 
      } else if (preset == "care_bears") {
        edges <<- detailed_edges
      }
    },
    testEdges = function() {
      message("Test if edge names contain required fields:")
      contains_expected_cols <- all(expected_edge_cols %in% colnames(edges))
      message(contains_expected_cols)
      message("Test if edges are non-empty:")
      test_non_empty <- (nrow(edges) > 0)
      message(test_non_empty)
      message("Edge test passing status:")
      edge_test_results <- all(contains_expected_cols, test_non_empty)
      return(edge_test_results)
    }
  )
)

# must initialiase nodes after edges
ButtonNodeDesignCategory <- setRefClass(
  "ButtonNodeDesignCategory",
  contains = "ButtonEdgeDesignCategory",
  fields = list(
    nodes = "data.frame",
    expected_node_cols = "character"
  ),
  methods = list(
    initialize = function(preset = "anal_beads") {
      callSuper()
      setEdges(preset)
      setNodes(preset)
    },
    setNodes = function(preset = "anal_beads"){
      expected_node_cols <<- c("node", "node_colour")

      if (preset %in% c("anal_beads", "hairy_anal_beads")) {
        nodes <<- tibble::tibble(
          node = c(edges$to, edges$from)
        ) |>
        dplyr::distinct() |>
        dplyr::mutate(
          node_colour = dplyr::case_when(
              node %in% c("source") ~ "project planning",
              node %in% c("analyse", "interpret") ~ "data analysis",
              node %in% c("decision", "question") ~ "decision making",
              TRUE  ~ "data engineering"
        ))} else if (preset == "care_bears") {
          nodes <<- detailed_nodes
        }
        },
    testNodes = function() {
      message("Test if node names contain required fields:")
      contains_expected_cols <- all(expected_node_cols %in% colnames(nodes))
      message(contains_expected_cols)
      message("Test if nodes are non-empty:")
      test_non_empty <- (nrow(nodes) > 0)
      message(test_non_empty)
      message("Edge test passing status:")
      node_test_results <- all(contains_expected_cols, test_non_empty)
      return(node_test_results)
    }
  )
)

## Vis




# Set relational integrity

# Set the design category
ButtonDesignCategory <- setRefClass(
  "ButtonDesignCategory",
  contains = "ButtonNodeDesignCategory",
  fields = list(
    nodes = "data.frame",
    edges = "data.frame"
  ),
  methods = list(
    initialize = function(preset = "anal_beads") {
      callSuper(preset = preset)
      setNodes(preset)
    },

    generateDotGraph = function(alpha = "B3") {
      role_to_color <- function(role, alpha = "CC") {
        base_hex <- dplyr::case_when(
          role == "data engineering"   ~ "#ADD8E6",
          role == "data analysis"      ~ "#8FBC8F",
          role == "project planning"   ~ "#F5DEB3",
          role == "decision making"    ~ "#F08080",
          role == "platform engineering" ~ "#FF8C00",
          TRUE                         ~ "#D3D3D3"
        )
        paste0(base_hex, alpha)
      }

      generate_role_legend <- function(alpha = "CC") {
        roles <- c(
          "data engineering", 
          "data analysis", 
          "project planning", 
          "decision making",
          "platform engineering")
        tibble::tibble(
          role = roles,
          fillcolor = sapply(roles, role_to_color, alpha = alpha)
        )
      }

      node_lines <- apply(.self$nodes, 1, function(row) {
        paste0('"', row[["node"]], '" [style=filled, fillcolor="',
               role_to_color(row[["node_colour"]], alpha),
               '", shape=box];')
      })

      edge_lines <- apply(.self$edges, 1, function(row) {
        style <- ifelse(row[["line_type"]] == "unintended", "dashed", "solid")
        paste0('"', row[["from"]], '" -> "', row[["to"]], '" [style=', style, '];')
      })

      legend_df <- generate_role_legend(alpha)
      legend_nodes <- apply(legend_df, 1, function(row) {
        paste0('"', row[["role"]], '" [label="', row[["role"]],
               '", style=filled, fillcolor="', row[["fillcolor"]], '", shape=box];')
      })

      legend_cluster <- paste0(
        "subgraph cluster_legend {\n",
        "  label = \"Legend\";\n",
        "  style = dashed;\n",
        "  fontsize = 12;\n",
        "  margin = 20;\n",
        "  rankdir = LR;\n",
        "  rank = sink;\n",
        paste(legend_nodes, collapse = "\n  "), "\n  ",
        '"data engineering" -> "data analysis" -> "platform engineering" -> "project planning" -> "decision making" [style=invis];\n',
        "}"
      )

      dot <- paste0(
        "digraph {\n",
        "  graph [margin=0.05, bgcolor=\"transparent\", rankdir=TB, splines=true];\n",
        "  rankdir=LR;\n",
        "  node [fontname=Helvetica];\n\n  ",
        paste(node_lines, collapse = "\n  "), "\n\n  ",
        paste(edge_lines, collapse = "\n  "), "\n\n  ",
        legend_cluster, "\n}"
      )

      return(dot)
    }
  )
)
