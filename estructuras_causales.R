library(tidyverse)


N <- 1000

##### CHAIN #####

alpha <- 1
beta <- -2
beta_2 <- 3
alpha_2 <- -4

set.seed(123)

df <- tibble(
  X1 = rnorm(N),
  X2 = beta_2 * X1 + alpha_2 + rnorm(N, 0, 0.1),
  Y = beta * X2 + alpha + rnorm(N, 0, 0.1)
)

cor(df)

summary(lm(Y ~ X1 + X2, data = df))


### X1
# Efecto directo de X1 sobre Y
summary(lm(Y ~ X1 + X2, data = df))

# Efecto total de X1 sobre Y
summary(lm(Y ~ X1, data = df))

### X2
# Efecto directo de X2 sobre Y
summary(lm(Y ~ X2, data = df))

# Efecto total de X2 sobre Y
summary(lm(Y ~ X2, data = df))

### FORK ###

alpha <- 1
beta <- -2
beta_2 <- 3
alpha_2 <- -4

df <- tibble(
  X1 = rnorm(N),
  X2 = beta_2 * X1 + alpha_2 + rnorm(N, 0, 0.1),
  Y = beta * X1 + alpha + rnorm(N, 0, 0.1)
)

cor(df)

### X1
# Efecto directo de X1 sobre Y
summary(lm(Y ~ X1, data = df))

# Efecto total de X1 sobre Y
summary(lm(Y ~ X1, data = df))

### X2
# Efecto directo de X2 sobre Y
summary(lm(Y ~ X1 + X2, data = df))

# Efecto total de X2 sobre Y
summary(lm(Y ~ X1 + X2, data = df))

##### COLLIDER #####
beta_1 <- 3
alpha_1 <- -4
beta <- -2

df <- tibble(
  X2 = rnorm(N),
  Y = rnorm(N),
  X1 = beta_1 * X2 + beta * Y + alpha_1 + rnorm(N, 0, 0.1)
)

cor(df)

summary(lm(Y ~ X1 + X2, data = df))

### X1
# Efecto directo de X1 sobre Y
# ???

# Efecto total de X1 sobre Y
# ???

### X2
# Efecto directo de X2 sobre Y
summary(lm(Y ~ X2, data = df))

# Efecto total de X2 sobre Y
# Efecto directo de X1 sobre Y
summary(lm(Y ~ X2, data = df))

##### TV - Seearch - Sales
beta_tv_search <- 3
beta_tv_sales <- 2
beta_search <- 1
alpha_search <- 1
alpha_sales <- 1

df <- tibble(
  TV = rnorm(N),
  Search = beta_tv_search * TV + alpha_search + rnorm(N, 0, 1),
  Sales = beta_tv_sales * TV + beta_search * Search + alpha_sales + rnorm(N, 0, 0.1)
)

# Efectos reales
efecto_directo_tv <- beta_tv_sales
efecto_directo_tv
efecto_total_tv <- beta_tv_sales * beta_tv_search
efecto_total_tv

efecto_directo_search <- beta_search
efecto_directo_search
efecto_total_search <- beta_search
efecto_total_search

cor(df)

summary(lm(Sales ~ TV + Search, data = df))

### TV
# Efecto directo de TV sobre Sales
summary(lm(Sales ~ TV + Search, data = df))

# Efecto total de TV sobre Sales
summary(lm(Sales ~ TV, data = df))

### Search
# Efecto directo de Search sobre Sales
summary(lm(Sales ~ TV + Search, data = df))

# Efecto total de Search sobre Sales
summary(lm(Sales ~ TV + Search, data = df))


### Y si?
summary(lm(Sales ~ Search, data = df))
