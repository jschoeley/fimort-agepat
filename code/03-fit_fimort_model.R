#############
# FIT MODEL #
#############

library(ExtDist) # scaled beta distribution
library(maxLik)  # maximum likelihood estimation
library(dplyr)
library(ggplot2)
library(readr)

filt_doc <- read_csv("./out/data/lifetables/filt_doc_sex.csv")
source("./code/ggplot_stepwise_ribbon.R")

# Adaptation and Frailty Components ---------------------------------------

# Gompertz (cumulative) hazard
GHzrd <- function (x, a, b) {
  a*exp(b*x)
}
GCumHzrd <- function (x, a, b) {
  a/b * (exp(b*x) - 1)
}

# Gamma-Gompertz frailty model
GGPopHzrd <- function (x, a, b, gamma) {
  GHzrd(x, a, b) / (GCumHzrd(x, a, b)*gamma + 1)
}

# Birth Trauma Component --------------------------------------------------

BetaHzrd <- function (x, A, s1, s2, a = 0, b = 1) {
  A*dBeta_ab(x, s1, s2, a, b)
}

# Full Model --------------------------------------------------------------

TheModel <- function (x, pars) {
  a1 = pars[1]; b  = pars[2]; gamma = pars[3]
  a2 = pars[4]; s1 = pars[5]; s2    = pars[6]
  return(
    GGPopHzrd(x,
              a1, b, gamma) +
      BetaHzrd(x,
               a2, s1, s2,
               a = 23-23, b = 47-23)
  )
}

# Objective Function ------------------------------------------------------

# poisson likelihood vector
# see http://data.princeton.edu/wws509/notes/c7.pdf for a derivation of
# the likelihood function
PoisML <- function (pars, age, obsDx, obsNx, HzrdFnct) {

  # predict hazard on basis of parameter estimates
  predHzrd <- HzrdFnct(x = age, pars)

  # poisson log-likelihood vector
  return(obsDx*log(predHzrd) - obsNx*predHzrd)

}

# Fit Model ---------------------------------------------------------------

# subset to males, 2009
filt %>%
  filter(sex == "Male", date_of_conception_y == 2009, x >= 23) -> filt_male_2009

# initialize model parameters
init_pars <- c(a1    = 7.5e-04, # mortality level at week 23 (process time 0)
               b     = -0.1,    # relative rate of age mortality decline (adaptation)
               gamma = 100,     # variance of frailties at week 0
               a2    = 0.0001,  # added mortality risk due to birth
               s1    = 39-23,   # age where onset of labor is most likely
               s2    = 10)      # spread of onset of labor

#standard errors
#sqrt(diag(solve(-model_fit$hessian)))

# fit model
maxLik(
  logLik = PoisML,
  start  = init_pars,
  method = "BFGS",
  # data
  age = filt_male_2009$x-23,
  obsDx = filt_male_2009$fd+filt_male_2009$id,
  obsNx = filt_male_2009$f+filt_male_2009$i,
  # hazard function
  HzrdFnct = TheModel,
  # options
  iterlim = 10000
) -> filt_male_2009_fit

# Plot model --------------------------------------------------------------

# plot predicted versus observed mortality rates
plot_us_fimort_2009_mx_predobs <-
  ggplot(filt_male_2009, aes(x = x-23)) +
  # observed hazard
  geom_step(aes(y = fdid_hx)) +
  # predicted hazard
  stat_function(fun = TheModel,
                args = list(pars = filt_male_2009_fit$estimate),
                color = rcpal::rcpal$quacla[1], size = 1,
                n = 1000) +
  # scale
  scale_y_continuous("Weekly fetal-infant mortality rate",
                     breaks = c(1e-06, 1e-05, 1e-04, 1e-03),
                     trans = "log10",
                     limits = c(1e-06, 1e-03)) +
  scale_x_continuous("Weeks of gestational age",
                     breaks = c(23, 30, 40, 50, 60 ,70, 80, 90)-23,
                     labels = c(23, 30, 40, 50, 60 ,70, 80, 90),
                     limits = c(23, 92)-23) +
  # annotate
  annotate(geom = "text", x = 30, y = 1e-05,
           label = paste(format(filt_male_2009_fit$estimate,
                                scientific = TRUE, digits = 3),
                         collapse = "\n"),
           family = "sans") +
  # theme
  ggtheme::ggtheme_min(grid = "xy", base_family = "sans")

ggsave("./out/fig/filt_male_2009_fit.pdf",
       width = 5.76, height = 3.76)
