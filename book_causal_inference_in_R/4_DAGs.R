library(ggdag)
library(tidyverse)

dagify(
  podcast ~ mood + humor + prepared,
  exam ~ mood + prepared
)


podcast_dag <- dagify(
  podcast ~ mood + humor + prepared,
  exam ~ mood + prepared,
  coords = time_ordered_coords(
    list(
      # time point 1
      c("prepared", "humor", "mood"),
      # time point 2
      "podcast",
      # time point 3
      "exam"
    )
  ),
  exposure = "podcast",
  outcome = "exam",
  labels = c(
    podcast = "podcast",
    exam = "exam score",
    mood = "mood",
    humor = "humor",
    prepared = "prepared"
  )
)

class(podcast_dag)

ggdag(podcast_dag, use_labels = "label", text = FALSE) +
  theme_dag()


theme_set(
  theme_dag() %+replace%
    # also add some additional styling
    theme(
      legend.position = "bottom",
      strip.text.x = element_text(margin = margin(2, 0, 2, 0, "mm"))
    )
)

# no coordinates specified
set.seed(123)
pod_dag <- dagify(
  podcast ~ mood + humor + prepared,
  exam ~ mood + prepared
)

# automatically determine layouts
pod_dag |>
  ggdag(text_size = 2.8)

pod_dag |>
  ggdag(layout = "sugiyama", text_size = 2.8)

pod_dag |>
  ggdag(layout = "time_ordered", text_size = 2.8)


# ggdag_paths() visualizes open paths in a DAG. 
# There are two open paths in podcast_dag: the fork from mood and the fork from prepared.
podcast_dag |>
  # show the whole dag as a light gray "shadow"
  # rather than just the paths
  ggdag_paths(shadow = TRUE, text = FALSE, use_labels = "label")

podcast_dag_tidy <- podcast_dag |>
  tidy_dagitty()

podcast_dag_tidy


podcast_dag_tidy |>
  dag_paths() |>
  filter(set == 2, path == "open path")

library(dagitty)

podcast_dag_tidy |>
  pull_dag() |>
  paths()


ggdag_adjustment_set(
  podcast_dag,
  text = FALSE,
  use_labels = "label"
)


podcast_dag_tidy |>
  # add adjustment sets to data
  dag_adjustment_sets() |>
  ggplot(aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
    color = adjusted,
    shape = adjusted
  )) +
  # ggdag's custom geoms: add nodes, edges, and labels
  geom_dag_point() +
  # remove adjusted paths
  # geom_dag_edges_link(data = \(.df) dplyr::filter(.df, adjusted != "adjusted")) +
  geom_dag_edges_link(data = \(.df) dplyr::filter(.df, adjusted != "adjusted")) +
  geom_dag_label_repel() +
  # you can use any ggplot function, too
  facet_wrap(~set) +
  scale_shape_manual(values = c(adjusted = 15, unadjusted = 19))


ggdag_adjustment_set(
  podcast_dag,
  text = FALSE,
  use_labels = "label",
  # get full adjustment sets
  type = "all"
)

adjustmentSets(podcast_dag, type = "canonical")

set.seed(10)
sim_data <- podcast_dag |>
  simulate_data()

sim_data

## Model that does not close backdoor paths
library(broom)

unadjusted_model <- lm(exam ~ podcast, sim_data) |>
  tidy(conf.int = TRUE) |>
  filter(term == "podcast") |>
  mutate(formula = "unadjusted")

## Model that closes backdoor paths
adjusted_model <- lm(exam ~ podcast + mood + prepared, sim_data) |>
  tidy(conf.int = TRUE) |>
  filter(term == "podcast") |>
  mutate(formula = "mood + prepared")

bind_rows(
  unadjusted_model,
  adjusted_model
) |>
  ggplot(aes(x = estimate, y = formula, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linewidth = 1, color = "grey80") +
  geom_pointrange(fatten = 3, size = 1) +
  theme_minimal(18) +
  labs(
    y = NULL,
    caption = "correct effect size: 0"
  )

podcast_dag_wrong <- dagify(
  podcast ~ humor + prepared,
  exam ~ prepared,
  coords = time_ordered_coords(
    list(
      # time point 1
      c("prepared", "humor"),
      # time point 2
      "podcast",
      # time point 3
      "exam"
    )
  ),
  exposure = "podcast",
  outcome = "exam",
  labels = c(
    podcast = "podcast",
    exam = "exam score",
    humor = "humor",
    prepared = "prepared"
  )
)

ggdag(podcast_dag_wrong, use_labels = "label", text = FALSE) +
  theme_dag()

## Model that does not close backdoor paths
library(broom)

unadjusted_model <- lm(exam ~ podcast, sim_data) |>
  tidy(conf.int = TRUE) |>
  filter(term == "podcast") |>
  mutate(formula = "unadjusted")

## Model that closes backdoor paths
adjusted_model <- lm(exam ~ podcast + prepared, sim_data) |>
  tidy(conf.int = TRUE) |>
  filter(term == "podcast") |>
  mutate(formula = "prepared")

bind_rows(
  unadjusted_model,
  adjusted_model
) |>
  ggplot(aes(x = estimate, y = formula, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linewidth = 1, color = "grey80") +
  geom_pointrange(fatten = 3, size = 1) +
  theme_minimal(18) +
  labs(
    y = NULL,
    caption = "correct effect size: 0"
  )
