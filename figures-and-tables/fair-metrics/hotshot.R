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

# Read and clean data
dat <- read_tsv(here("figures-and-tables", "fair-metrics", "hotshot.tsv"), skip = 1) |>
  cleanup_colnames()

# Long form for plotting
dat_long <- dat |>
  pivot_longer(cols = -stakeholders, names_to = "timepoint", values_to = "fair_metric") |>
  mutate(
    tp_number = str_extract(timepoint, '\\d+') |> as.integer()
  )

# Create ordered factor for vertical alignment
tp_levels <- sort(unique(dat_long$tp_number), decreasing = TRUE)

dat_long <- dat_long |>
  mutate(
    time_f = factor(tp_number, levels = tp_levels),
    fair_label = case_when(
      is.na(fair_metric) ~ "Not on project",
      fair_metric == 0   ~ "No access to analysis",
      fair_metric == 1   ~ "Has access to analysis"
    ) |> factor(levels = c("Not on project", "No access to analysis", "Has access to analysis")),
    alpha_level = if_else(fair_label == "Has access to analysis", 0.7, 0.3),
    y = as.numeric(time_f)
  )

tp_annotations <- tibble(
  timepoint = colnames(dat),
  annotation = colnames(read_tsv(here("figures-and-tables", "fair-metrics", "hotshot.tsv")))
) |>
  mutate(
    tp_number = str_extract(timepoint, '\\d+') |> as.integer(),
    time_f = factor(tp_number, levels = tp_levels),
    plot_y = as.numeric(time_f),
    annotation = if_else(str_length(annotation) < 6, "...", annotation),
    annotation = str_to_sentence(annotation),
    annotation = str_wrap(annotation, 40)
  )

# Define color palette
murky_palette_gg <- c(
  "Not on project"        = "#BCAAA4",
  "No access to analysis" = "#800020",
  "Has access to analysis"= "#0D47A1"
)

fair_prop <- 
  dat_long |>
    filter(!is.na(fair_metric)) |>
    summarise(
      fair = sum(fair_metric),
      total = n(),
      prop = round(fair / total, 2)
    )

plot_cap <- sprintf(
  "FAIR access available in %i of %i cases (%i per cent of on project instances).",
  fair_prop$fair,
  fair_prop$total,
  fair_prop$prop * 100
) |> str_wrap(30)


# Plot
ggplot(dat_long) +
  geom_point(aes(x = stakeholders, y = y, shape = stakeholders, colour = fair_label, alpha = alpha_level), size = 7) +
  geom_text(data = tp_annotations |> mutate(stakeholders = "Principal Investigator"),
            aes(x = stakeholders,
            family="Courier", 
            y = plot_y, label = annotation)) +
  scale_colour_manual(values = murky_palette_gg, name = "FAIR Metric") +
  scale_alpha_identity() +
  theme_minimal(base_family = "Courier", base_size = 14) +
  labs(
    title = "Hotshot in the Lab",
    subtitle = "FAIR access to analysis is prevented for most of the project" |> str_wrap(40), 
    x = NULL, y = "Time (Top to Bottom)",
    caption = plot_cap
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

# Save
ggsave(here("figures-and-tables", "fair-metrics", "fair-access.png"),
       units = "in", width = 6, height = 12)
