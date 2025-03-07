library(tidyverse)
library(gt)

# construct the table data
dat <- 
  read_delim("figures-and-tables/fair-minimum-criteria/fair-minimum-dat.delim", 
    delim = '--')

# create gt
dat_gt <- dat  %>% 
  gt()  %>% 
  tab_header(md("Validation of Data Entity meets Minimum [FAIR](https://www.go-fair.org/fair-principles/) Criteria"))

# write to manuscript directory
gtsave(dat_gt, "slidedeck/fair-minimum-criteria.html")
gtsave(dat_gt, "ps-and-qs-manuscript/fair-data-entity.html")
