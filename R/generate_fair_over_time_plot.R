library(tidyverse)
library(here)

# Cleanup function
cleanup_colnames <- function(df) {
  names(df) <- names(df) |>
    tolower() |>
    (\(x) gsub("[^a-z0-9]+", "_", x))() |>
    (\(x) gsub("^_|_$", "", x))() |>
    (\(x) ifelse(grepl("^[0-9]", x), paste0("x", x), x))()
  df
}

# create a generator function for the pictor
FairPicto <- setRefClass(
  "FairPicto",

  # attributes of the class
  fields = list(
  dat_path = "character",
  dat_file = "character",
  input_dat = "data.frame",
  tp_levels = "numeric",
  stakeholders_n = "numeric"  
),

  # functions 
  methods = list(
    initialize = function(dat_path = here("figures-and-tables", "fair-metrics"), dat_file) {
      .self$input_dat <- read_tsv(here(dat_path, dat_file), skip = 1) |>
      cleanup_colnames()

      .self$dat_path <- dat_path
      .self$dat_file <- dat_file
      .self$stakeholders_n <- nrow(.self$input_dat)
      
    },
    go_long = function(){

      # Long form for plotting
      dat_long <- input_dat |>
        pivot_longer(cols = -stakeholders, names_to = "timepoint", values_to = "fair_metric") |>
        mutate(
          tp_number = str_extract(timepoint, '\\d+') |> as.integer()
        )

      # Create ordered factor for vertical alignment
      .self$tp_levels <- sort(unique(dat_long$tp_number), decreasing = TRUE)

      dat_long <- dat_long |>
        mutate(
          time_f = factor(tp_number, levels = .self$tp_levels),
          fair_label = case_when(
            is.na(fair_metric) ~ "Not on project",
            fair_metric == 0   ~ "No access to analysis",
            fair_metric == 1   ~ "Has access to analysis"
          ) |> factor(levels = c("Not on project", "No access to analysis", "Has access to analysis")),
          alpha_level = 
            if_else(fair_label == "Has access to analysis", 
              0.5, 0.2),
          y = as.numeric(time_f)
        )
      
      return(dat_long)

    },
  tp_annotations = function() {
    tibble(
      timepoint = colnames(.self$input_dat),
    # we need to fix this
      annotation = colnames(read_tsv(here(.self$dat_path, .self$dat_file)))
) |>
  mutate(
    tp_number = str_extract(timepoint, '\\d+') |> as.integer(),
    time_f = factor(tp_number, levels = tp_levels),
    plot_y = as.numeric(time_f),
    annotation = if_else(str_length(annotation) < 6, "...", annotation),
    annotation = str_to_sentence(annotation),
    annotation = str_wrap(annotation, 60)
  )

},

  murky_palette = function() {
    c(
  "Not on project"        = "#BCAAA4",
  "No access to analysis" = "#800020",
  "Has access to analysis"= "#0D47A1"
)
  },

  fair_prop = function() {
    .self$go_long() |>
    filter(!is.na(fair_metric)) |>
    summarise(
      fair = sum(fair_metric),
      total = n(),
      prop = round(fair / total, 2)
    )

  },
  
  plot_cap = function() {
    fair_prop <- .self$fair_prop()

    sprintf(
      "FAIR access available in %i of %i cases (%i per cent of on project instances).",
      fair_prop$fair,
      fair_prop$total,
      fair_prop$prop * 100
    ) |> str_wrap(30)
  },

  plot_fair = function() {
    ggplot(.self$go_long()) +
  geom_point(aes(x = stakeholders, 
    y = y, 
    shape = stakeholders, 
    colour = fair_label, 
    alpha = alpha_level), size = 7) +
  geom_text(
    data = .self$tp_annotations() |> 
      mutate(stakeholders = "Principal Investigator"),
      family="Courier", 
      aes(
        x = 1,
        y = plot_y, 
        label = annotation),
        hjust = 0
        ) +
  scale_colour_manual(values = .self$murky_palette(), name = "FAIR Metric") +
  scale_alpha_identity() +
  theme_minimal(base_family = "Courier", base_size = 20) +
  labs(
    title = "Hotshot in the Lab",
    subtitle = "FAIR access to analysis is prevented for most of the project" |> str_wrap(40), 
    x = NULL, y = "Time (Top to Bottom)",
    caption = .self$plot_cap()
    ) +
  theme(
    legend.direction = "vertical",
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.background = element_rect(colour = "white"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle=90)
  )
  },

  # another spot to update

  write_plot = function() {
    ggsave(here(
      .self$dat_path, 
      str_replace(.self$dat_file, '\\.\\w+', '.png')
      ),
    plot = .self$plot_fair(),
       units = "in", width = 8, height = 12)

  }  
)
)

# instantiate hotshot in class
HotshotPicto <- FairPicto(
  dat_file = "hotshot.tsv",
  pict_title = ""
  )

HotshotPicto$input_dat
HotshotPicto$go_long()
HotshotPicto$tp_annotations()
HotshotPicto$murky_palette()
HotshotPicto$fair_prop()
HotshotPicto$plot_fair()
HotshotPicto$write_plot()

# instantiate shitty pm in class
ShittyPMPicto <- FairPicto(
  dat_file = "shittypm.tsv"
  )

ShittyPMPicto$input_dat
ShittyPMPicto$go_long()
ShittyPMPicto$tp_annotations()
ShittyPMPicto$murky_palette()
ShittyPMPicto$fair_prop()
ShittyPMPicto$plot_fair()
ShittyPMPicto$write_plot()

