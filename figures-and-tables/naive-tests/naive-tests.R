library(tidyverse)
library(gt)

dat <- tribble(
  ~product_type,	~layer,	~Test,	~Tested,
  "CADP",	"published",	"unique key",	"data entity",
  "EDP",	"published",	"unique key",	"data entity joined across raw sources and tested",
  "SADP",	"published",	"unique key",	"data entity defined and tested",
  "SADP",	"source",	"unique key",	"combination of columns that define a unique row",
  "SADP",	"source",	"freshness",	"incrementation or snapshot field",
  "SADP",	"source",	"not empty",	"table-level test"
)

data_products <- tribble(
  ~product_type, ~data_layer,
  "SADP", "Source",
  "EDP", "Semantic Transformation",
  "CADP", "Analytical Observation"
) 

data_layers <- tribble(
  ~layer, ~io_layer,
  'source', 'input',
  'published', 'output'
)


test_gt <- dat |>
# relabel for general audience
  left_join(data_products, by = "product_type")  %>% 
  left_join(data_layers, by = "layer")  %>% 
  mutate(product_type = data_layer, layer = factor(io_layer, levels = c("input", "output")))  %>% 
  select(-data_layer, -io_layer)  %>% 
  gt(rowname_col = "layer", groupname_col = "product_type") %>%
  tab_source_note(md("We say a unique key has been tested when the same combination of columns have not null  and unique tests applied."))  %>% 
  tab_header("Tests Applied on Data Product Layers by Observability, Descending")  %>% 
  tab_source_note("Freshness tests configurations: daily ingestion (warn > 1 day, error > 1 week); weekly ingestion (warn > 1 week, error > 2 weeks)")  %>% 
  tab_options(
    table.background.color = "transparent",
    container.overflow.x = TRUE,
    container.overflow.y = TRUE
  )

# Save the gt table as an HTML file first
gtsave(test_gt, "slidedeck/naive-tests.html")
gtsave(test_gt, "ps-and-qs-manuscript/data-entity-tests.html")
