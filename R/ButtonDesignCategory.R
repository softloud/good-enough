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
)

# I expect there to be an edge from transform to analyse
# and transform to be labelled as analytics engineering
# and analyse to be labelled as data analytics

# Edges methods
ButtonEdgeDesignCategory <- setRefClass(
  "ButtonEdgeDesignCategory",
  fields = list(
    expected_edge_cols = "character",
    edges = "data.frame"
  ),
  methods = list(
    initialize = function(preset = "anal_beads") {
      callSuper()
      setEdges(preset)
    },
    setEdges = function(preset = "anal_beads") {
      expected_edge_cols <<- c("from", "to")

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
            reason = stringr::str_wrap(reason, 15)
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
      setNodes(preset)
    },
    setNodes = function(preset = "anal_beads"){
      expected_node_cols <<- c("node")

      nodes <<- tibble::tibble(
        node = c(edges$to, edges$from)
      ) |>
      dplyr::distinct()
    
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



# Set relational integrity

# Set the design category
ButtonDesignCategory <- setRefClass(
  "ButtonDesignCategory",
  contains = c("ButtonNodeDesignCategory"),
  fields = list(
    preset = "character"
  ),
  methods = list(
    initialize = function(preset = "anal_beads") {
      preset <<- preset
      callSuper(preset = preset)  
    }
  )
)
