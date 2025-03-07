library(tidyverse)
library(gt)

img <- "figures-and-tables/coconuts-table/crazy-in-the-coconut.jpeg"

expected_obs <- 
  tibble(
    x = seq(1, 5),
    y = seq(1, 5)
  )  %>% 
    mutate(
      x = str_c("$x_", x, "$"),
      y = str_c("$y_", y, "$")
    )  %>% 
    gt() %>%
    fmt_markdown(
      columns = c(x, y)
    )  %>% 
    tab_header("Expection") 
    
expected_obs

gtsave(expected_obs, "ps-and-qs-manuscript/expected-obs.html")

coconut_obs <- 
  tibble(
    x = seq(1, 5),
    y = seq(1, 5)
  )  %>%
  gt() |>
  text_transform(
    locations = cells_body(columns = c(x, y)),
    fn = function(x) {
      local_image(
        filename = img
      )
    }
  )  %>%
  tab_header("Reality")

coconut_obs

gtsave(coconut_obs, "ps-and-qs-manuscript/coconut-obs.html")
