library(tidyverse)
library(causalworkshop)

theme_set(
  theme_dag() %+replace%
    # also add some additional styling
    theme(
      legend.position = "bottom",
      strip.text.x = element_text(margin = margin(2, 0, 2, 0, "mm"))
    )
)


# 1 - Specify a causal question -------------------------------------------



# id
# an ID variable
# 
# net and net_num
# a binary variable indicating if the participant used a net (1) or didn’t use a net (0)
# 
# malaria_risk
# risk of malaria scale ranging from 0-100
# 
# income
# weekly income, measured in dollars
# 
# health
# a health score scale ranging from 0–100
# 
# household
# number of people living in the household
# 
# eligible
# a binary variable indicating if the household is eligible for the free net program.
# 
# temperature
# the average temperature at night, in Celsius
# 
# resistance
# Insecticide resistance of local mosquitoes. A scale of 0–100, with higher values indicating higher resistance.

net_data

net_data |>
  ggplot(aes(malaria_risk, fill = net)) +
  geom_density(color = NA, alpha = .8)

net_data |>
  group_by(net) |> 
  summarize(malaria_risk = mean(malaria_risk))

library(broom)

net_data |>
  lm(malaria_risk ~ net, data = _) |>
  tidy()

# 2 - Draw our assumptions using a causal diagram -------------------------

library(ggdag)

net_dag <- dagify(
  malaria_risk ~ net + income + temperature + resistance + health,
  net ~ temperature + income + eligible + household + health,
  health ~ income,
  eligible ~ household + income,
  # coords = time_ordered_coords(
  #   list(
  #     c("household"),
  #     "eligible",
  #     "net",
  #     "income",
  #     "health",
  #     "temperature",
  #     "malaria_risk",
  #     "resistance"
  #   )
  # ),
  coords = list(
    x = c(
      malaria_risk = 7,
      net = 3,
      income = 4,
      health = 5,
      temperature = 6,
      resistance = 8.5,
      eligible = 2,
      household = 1
    ),
    y = c(
      malaria_risk = 2,
      net = 2,
      income = 3,
      health = 1,
      temperature = 3,
      resistance = 2,
      eligible = 3,
      household = 2
    )
  ),
  exposure = "net",
  outcome = "malaria_risk",
  labels = c(
    household = "Number in the household",
    eligible = "Elegible for free net",
    net = "Mosquito net use",
    income = "Income",
    health = "Health",
    temperature = "Night temperature",
    malaria_risk = "Risk of malaria",
    resistance = "Insecticide resistance"
  ) 
)

adjustmentSets(net_dag, type = "minimal")

ggdag(net_dag, use_labels = "label", text = FALSE) 


# automatically determine layouts
net_dag |>
  ggdag(text_size = 2.8)

net_dag |>
  ggdag(layout = "sugiyama", text_size = 2.8)

net_dag |>
  ggdag(layout = "time_ordered", text_size = 2.8)


net_dag |>
  ggdag_paths(shadow = TRUE, text = FALSE, use_labels = "label")

ggdag_adjustment_set(
  net_dag,
  text = FALSE,
  use_labels = "label"
)


# 3 - Model our assumptions -----------------------------------------------



propensity_model <- glm(
  net ~ income + health + temperature,
  data = net_data,
  family = binomial()
)

# the first six propensity scores
head(predict(propensity_model, type = "response"))

library(broom)
library(propensity)

net_data_wts <- propensity_model |>
  augment(data = net_data, type.predict = "response") |>
  # .fitted is the value predicted by the model
  # for a given observation
  mutate(wts = wt_ate(.fitted, net))

net_data_wts |>
  select(net, .fitted, wts) |>
  head()


# 4 - Diagnose our models -------------------------------------------------


library(halfmoon)

ggplot(net_data_wts, aes(.fitted)) +
  geom_mirror_histogram(
    aes(fill = net),
    bins = 50
  ) +
  scale_y_continuous(labels = abs) +
  labs(x = "propensity score")


ggplot(net_data_wts, aes(.fitted)) +
  geom_mirror_histogram(
    aes(group = net),
    bins = 50
  ) +
  geom_mirror_histogram(
    aes(fill = net, weight = wts),
    bins = 50,
    alpha = .5
  ) +
  scale_y_continuous(labels = abs) +
  labs(x = "propensity score")


plot_df <- tidy_smd(
  net_data_wts,
  c(income, health, temperature),
  .group = net,
  .wts = wts
)

ggplot(
  plot_df,
  aes(
    x = abs(smd),
    y = variable,
    group = method,
    color = method
  )
) +
  geom_love()


net_data_wts |>
  ggplot(aes(wts)) +
  geom_density(fill = "#CC79A7", color = NA, alpha = 0.8)

# 5 - Estimate the causal effect ----------------------------------------------

net_data_wts |>
  lm(malaria_risk ~ net, data = _, weights = wts) |>
  tidy(conf.int = TRUE)


library(rsample)

fit_ipw <- function(.split, ...) {
  # get bootstrapped data frame
  .df <- as.data.frame(.split)
  
  # fit propensity score model
  propensity_model <- glm(
    net ~ income + health + temperature,
    data = .df,
    family = binomial()
  )
  
  # calculate inverse probability weights
  .df <- propensity_model |>
    augment(type.predict = "response", data = .df) |>
    mutate(wts = wt_ate(.fitted, net))
  
  # fit correctly bootstrapped ipw model
  lm(malaria_risk ~ net, data = .df, weights = wts) |>
    tidy()
}

bootstrapped_net_data <- bootstraps(
  net_data,
  times = 1000,
  # required to calculate CIs later
  apparent = TRUE
)

bootstrapped_net_data

ipw_results <- bootstrapped_net_data |>
  mutate(boot_fits = map(splits, fit_ipw))

ipw_results

ipw_results$boot_fits[[1]]

ipw_results |>
  # remove original data set results
  filter(id != "Apparent") |> 
  mutate(
    estimate = map_dbl(
      boot_fits,
      # pull the `estimate` for `netTRUE` for each fit
      \(.fit) .fit |>
        filter(term == "netTRUE") |>
        pull(estimate)
    )
  ) |>
  ggplot(aes(estimate)) +
  geom_histogram(fill = "#D55E00FF", color = "white", alpha = 0.8)

boot_estimate <- ipw_results |>
  # calculate T-statistic-based CIs
  int_t(boot_fits) |>
  filter(term == "netTRUE")

boot_estimate


# 6 - Conduct sensitivity analysis on the effect estimate -----------------


