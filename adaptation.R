library(lme4)
library(tidyverse)
library(ggplot2)
library(rstan)
library(bayesplot)

source("~/hessian/hessian_helper.R")

fit = stan("models/funnel_neal.stan", chains = 1)

load_model("models/funnel_neal.stan", list())

jacobian(c(1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0))

extract(fit, pars = c("y", "x")) %>%
  (function(o) { cbind(o$y, o$x) }) %>%
  cbind(., apply(., 1, function(x) { 1.0 / sqrt(sum(jacobian(x)$jac^2)) })) %>%
  as.tibble -> df1

df1 %>%
  mutate(expy = exp(V1 / 2.0)) %>%
  pairs(pch = 16, cex = 0.4)

lm(V4 ~ V1 + V2 + V3, df1)

df1 %>%
  mutate(expy = exp(y / 2.0)) %>%
  filter(expy < 1e1) %>%
  ggplot(aes(expy, dt)) +
  geom_point()

df1 %>%
  ggplot(aes(x1, dt)) +
  geom_point()

df1 %>%
  ggplot(aes(x2, dt)) +
  geom_point()

posterior <- extract(fit, inc_warmup = TRUE, permuted = FALSE)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  n_warmup = 300,
                facet_args = list(nrow = 2, labeller = label_parsed))
p + facet_text(size = 15)
