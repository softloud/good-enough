source("R/ButtonDesignCategory.R")

# presets

# Initilisation method
initButton <- function() {
}


# Button generator

ButtonCategory <- setRefClass(
  "ButtonCategory",
  contains  = "ButtonDesignCategory",
  methods = list(
    initialize = initButton
  )
)

