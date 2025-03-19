library(tidyverse)
library(gt)

# construct the table data
dat <- 
  read_delim("figures-and-tables/fair-minimum-criteria/fair-minimum-dat.delim", 
    delim = '--')

# create gt
fair_gt <- dat  %>% 
  gt()  %>% 
  tab_header("Validation of Data Entity meets Minimum FAIR Criteria")

