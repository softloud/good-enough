# presets

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

# Initilisation method
initButton <- function() {
  if (length(preset) == 0) {
      preset <<- "anal_beads"
      edges <<- setEdges(preset)
  }
  }

# Edges methods

setEdges <- function(preset) {
  if (preset == "anal_beads") {
    edges <<- anal_beads_df
  }
} 



# Button generator

ButtonGenerator <- setRefClass(
  "ButtonGenerator",
  fields = list(
    preset = "character",
    nodes = "data.frame",
    edges = "data.frame"
  ),
  methods = list(
    initialize = initButton,
    setEdges = setEdges
  )
)

