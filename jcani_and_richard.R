### https://muestrear-no-es-pecado.es/2024/05/ensalada_no_saludable
### https://elevanth.org/blog/2021/06/15/regression-fire-and-dangerous-things-1-3/
### https://elevanth.org/blog/2021/06/21/regression-fire-and-dangerous-things-2-3/
### https://elevanth.org/blog/2021/06/29/regression-fire-and-dangerous-things-3-3/


library(tidyverse)
library(rstan)

# Simulate some data
m <- 0
b <- 2
k <- 1
p <- 0.5

set.seed(1908)
N <- 10000 # number of pairs
U <- rnorm(N) # simulate confounds
# birth order and family sizes
B1 <- rbinom(N, size=1, prob=p) # 50% first borns
M  <- rnorm( N, b*B1 + k*U )
B2 <- rbinom(N, size=1, prob=p)
D  <- rnorm( N, b*B2 + k*U + m*M ) # change the 0 to turn on causal influence of mom

df <- tibble(M = M, D = D, B1 = B1, B2 = B2, U = U)
df

summary(lm(D ~ M, data = df))
summary(lm(D ~ M + B1 + B2, data = df))



library(rstan)

# Definir el modelo como un string
stan_model_string <- "
data {
  int<lower=1> N;            // Número de observaciones
  int<lower=0, upper=1> B1[N];  // Variable binaria B1 como enteros
  int<lower=0, upper=1> B2[N];  // Variable binaria B2 como enteros
  vector[N] M;              // Variable mediadora M como vector real
  vector[N] D;              // Variable dependiente D como vector real
}

parameters {
  real alpha1;               // Intercepto para M
  real alpha2;               // Intercepto para D
  real b;                    // Coeficiente para B1 y B2
  real m;                    // Coeficiente para M en D
  real<lower=0> k;           // Coeficiente para U
  real<lower=0> sigma;       // Desviación estándar para M
  real<lower=0> tau;         // Desviación estándar para D
  real<lower=0, upper=1> p;  // Parámetro para Bernoulli
  vector[N] U;               // Variable latente U como vector real
}

model {
  // Priors
  alpha1 ~ normal(0, 0.5);
  alpha2 ~ normal(0, 0.5);
  b ~ normal(0, 0.5);
  m ~ normal(0, 0.5);
  k ~ exponential(1);
  sigma ~ exponential(1);
  tau ~ exponential(1);
  p ~ beta(2, 2);

  // Likelihood
  U ~ normal(0, 1);                    // Prior para U
  B1 ~ bernoulli(p);                   // Likelihood para B1
  B2 ~ bernoulli(p);                   // Likelihood para B2
  M ~ normal(alpha1 + b * to_vector(B1) + k * U, sigma);
  D ~ normal(alpha2 + b * to_vector(B2) + m * M + k * U, tau);
}

generated quantities {
  vector[N] y_rep_M;  // Predicciones simuladas para M
  vector[N] y_rep_D;  // Predicciones simuladas para D

  // Generar datos simulados a partir de la posterior
  for (n in 1:N) {
    y_rep_M[n] = normal_rng(alpha1 + b * to_vector(B1)[n] + k * U[n], sigma);
    y_rep_D[n] = normal_rng(alpha2 + b * to_vector(B2)[n] + m * M[n] + k * U[n], tau);
  }
}
"

# Compilar el modelo desde el string
stan_model_compiled <- stan_model(model_code = stan_model_string)

data_list <- list(
  N = nrow(df),
  B1 = df$B1,
  B2 = df$B2,
  M = df$M,
  D = df$D
)

# Ajustar el modelo
fit <- sampling(
  object = stan_model_compiled,
  data = data_list,
  iter = 2000,
  chains = 4,
  cores = 4
)

# Resumen de los resultados
print(fit, pars = c("alpha1", "alpha2", "b", "m", "k", "sigma", "tau", "p"))

## PP Check
# Extraer predicciones simuladas
posterior_samples <- as.data.frame(fit)

# Extraer las predicciones simuladas para M y D
y_rep_M <- as.matrix(posterior_samples[, grep("y_rep_M", colnames(posterior_samples))])
y_rep_D <- as.matrix(posterior_samples[, grep("y_rep_D", colnames(posterior_samples))])

library(bayesplot)
# Graficar los checks para M
ppc_dens_overlay(df$M, y_rep_M)

# Graficar los checks para D
ppc_dens_overlay(df$D, y_rep_D)

# Histograma para M
ppc_hist(df$M, y_rep_M)

# Histograma para D
ppc_hist(df$D, y_rep_D)

# Comparar medias
ppc_stat(df$M, y_rep_M, stat = "mean")
ppc_stat(df$D, y_rep_D, stat = "mean")

# Comparar desviaciones estándar
ppc_stat(df$M, y_rep_M, stat = "sd")
ppc_stat(df$D, y_rep_D, stat = "sd")

samples <- rstan::extract(fit, pars=c("alpha1", "alpha2", "b", "m", "k"))
samples_U <- rstan::extract(fit, pars=c("U"))

#### Calcula 4000 muestras de 1 única observación en la que lo único que cambia es B1

# B1 = 0
M_B1_0 <- with(samples, alpha1 + b * 0 + k * 0)
D_B1_0 <- with(samples, alpha2 + b * 0 + k * 0 + m*M_B1_0)

# B1 = 1
M_B1_1 <- with(samples, alpha1 + b * 1 + k * 0)
D_B1_1 <- with(samples, alpha2 + b * 0 + k * 0 + m*M_B1_1)

TE <- D_B1_1 - D_B1_0

hist(TE)

quantile(TE)

# Por ser lineal, sale lo mismo así:
quantile(with(samples,b*m))

### Calcula 4000 simulaciones de cada 1 de las N observaciones

arr_samples <- sapply(samples, function(x) x) %>% t

probe_M <- df %>% 
  select(B1) %>%
  mutate(intecpt_M = 1, .before = "B1")

probe_D <- df %>% 
  select(B2) %>%
  mutate(intecpt_D = 1, .before = "B2")

M_B1_0 <- as.matrix(probe_M %>% mutate(B1=0)) %*% arr_samples[c('alpha1', "b"),] + t(sweep(samples_U$U, 1, arr_samples['k',], "*"))
D_B1_0 <- as.matrix(probe_D) %*% arr_samples[c('alpha2', 'b'),] + M_B1_0 * arr_samples['m',] + t(sweep(samples_U$U, 1, arr_samples['k',], "*"))

M_B1_1 <- as.matrix(probe_M %>% mutate(B1=1)) %*% arr_samples[c('alpha1', "b"),] + t(sweep(samples_U$U, 1, arr_samples['k',], "*"))
D_B1_1 <- as.matrix(probe_D) %*% arr_samples[c('alpha2', 'b'),] + M_B1_1 * arr_samples['m',] + t(sweep(samples_U$U, 1, arr_samples['k',], "*"))

TE = D_B1_1 - D_B1_0

TE %>% apply(2, quantile) %>% apply(1, median)

ATE = apply(TE, 2, median)

quantile(ATE)

quantile(arr_samples['b',] * arr_samples['m',])


# Ajuste
M_hat <- as.matrix(probe_M) %*% arr_samples[c('alpha1', "b"),] + t(sweep(samples_U$U, 1, arr_samples['k',], "*"))
D_hat <- as.matrix(probe_D) %*% arr_samples[c('alpha2', 'b'),] + M_B1_0 * arr_samples['m',] + t(sweep(samples_U$U, 1, arr_samples['k',], "*"))

res <- df %>% select(M,D) %>% 
  mutate(M_hat_median = M_hat %>% apply(1, median), .after = "M") %>% 
  mutate(D_hat_median = D_hat %>% apply(1, median), .after = "D")

res %>% 
  ggplot(aes(x = M, y = M_hat_median)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "M")

res %>%
  ggplot(aes(x = D, y = D_hat_median)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "D")

samples_ppc <- rstan::extract(fit, pars=c("y_rep_M", "y_rep_D"))

res <- res %>% 
  mutate(M_hat_ppc = samples_ppc$y_rep_M %>% apply(2, median), .after = "M_hat_median") %>%
  mutate(D_hat_ppc = samples_ppc$y_rep_D %>% apply(2, median), .after = "D_hat_median")

res %>%
  ggplot(aes(x = M, y = M_hat_ppc)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "M PPC")

res %>%
  ggplot(aes(x = D, y = D_hat_ppc)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "D PPC")

###
res %>% 
  ggplot(aes(x = M_hat_median, y = M_hat_ppc)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "M_hat median vs PPC")

res %>%
  ggplot(aes(x = D_hat_median, y = D_hat_ppc)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "D_hat median vs PPC")


######
probe_ates <- tibble(ATE = ATE, ATE_m_b = arr_samples['b',] * arr_samples['m',])

# Densities in the same plot
probe_ates %>% 
  gather(key = "type", value = "value") %>%
  ggplot(aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "ATEs") +
  theme_minimal() + 
  xlim(-0.02, 0.02)
