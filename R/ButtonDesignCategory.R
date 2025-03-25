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
initEdges <- function() {
  expected_edge_cols <<- c("from", "to")

  setEdges(preset) 
} 

setEdges <- function(update_type){
  if (update_type == "anal_beads") {
    edges <<- anal_beads_df
  }

}

msg_r_object <- function(r_object) {
  message(paste0(capture.output(r_object), collapse = "\n"))
}



testEdges <- function() {
  
  message("Test if edge names contain required fields")
  contains_expected_cols <- all(colnames(edges) %in% expected_edge_cols)
  message(contains_expected_cols)
  message("Test if edges are non-empty")
  test_non_empty <- (nrow(edges) > 0)
  message(test_non_empty)
  message("Edge test passing status:")
  edge_test_results <- all(contains_expected_cols, test_non_empty)
  return(edge_test_results)
}

ButtonEdgeDesignCategory <- setRefClass(
  "ButtonEdgeDesignCategory",
  fields = list(
    expected_edge_cols = "character",
    edges = "data.frame"
  ),
  methods = list(
    initialize = initEdges,
    setEdges = setEdges,
    testEdges = testEdges
  )
)



# Set relational integrity

# Set the design category
initButtonDesign <- function() {
  if (length(preset) == 0) {
      preset <<- "anal_beads"      
  }
  initEdges() 
} 

ButtonDesignCategory <- 
  setRefClass("ButtonDesignCategory",
  contains = c("ButtonEdgeDesignCategory"),
  fields = list(
    preset = "character",
    node_design = "data.frame",
    nodes = "data.frame"
  ),
  methods = list(
    initialize = initButtonDesign
  )
  )