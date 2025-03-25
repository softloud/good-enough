source("R/ButtonDesignCategory.R")

# presets

# Initilisation method
initButton <- function(preset = "anal_beads") {
  callSuper(preset = preset)
}


# Button generator

ButtonCategory <- setRefClass(
  "ButtonCategory",
  contains  = "ButtonDesignCategory",
  methods = list(
    initialize = initButton
  )
)

