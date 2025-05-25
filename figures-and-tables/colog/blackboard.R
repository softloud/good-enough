





layer_1 <- ExtractLayer$new("figures-and-tables/colog/colog_input.ods")
layer_2 <- SemanticLayer$new(layer_1$get_nodes(), layer_1$get_edges())
layer_3 <- DotMaker$new(layer_2$nodes, layer_2$edges) 

layer_3$print_dot()

layer_1$get_nodes()
