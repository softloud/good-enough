anal_bead_theme <- function(plotdat) {
  list(
    geom_node_label(
      aes(label = name, fill = role),
      colour = "darkblue",
      family = "courier",
      size = 4,
      alpha = 0.3
    ),
    theme_minimal(),
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(
        size = 20,
        family = "courier",
        colour = "darkblue"
      ),
      plot.caption = element_text(
        hjust = 0.5, 
        size = 10,
        family = "courier",
        colour = "darkblue"
      ),
      plot.subtitle = element_text(
        size = 10,
        family = "courier",
        colour = "darkblue"
      ),
      legend.text = element_text(
        size = 10,
        family = "courier",
        colour = "darkblue"
      ),
      legend.title = element_text(
        size = 10,
        family = "courier",
        colour = "darkblue"
      )
    ))

} 