###############################
# CALCULATE INFANT LIFETABLES #
###############################

# Based on the individual level data on births and infant deaths we calculate
# infant lifetables for various groups

library(dplyr)
library(readr)
library(ggplot2)

load("./priv/data/processed_microdata/us_fideath_1989-2010.RData")
source("./code/ggplot_stepwise_ribbon.R")

# subset to infant deaths
fideath %>% filter(type == "infant") -> ideath

# The cohort lifetable function for individual level data -----------------

#' Calculate A Cohort Lifetable From Individual Survival Times
#'
#' TODO: smooting option for Dx, Nx and Cx
CohortLT <- function (time, event, alpha = 0.95) {
  require(data.table)
  # table of events and censorings for each point in time
  # use of data table for speedy operation on large data sets
  DxCx <- setkey(data.table(time, event), time)[,
                                                list(Dx  = sum(event),
                                                     Cx  = sum(!event)),
                                                by = time]

  # number of intervals
  J = nrow(DxCx)
  # number of events in interval x
  Dx  = DxCx$Dx
  # numberof censorings in interval x
  Cx  = DxCx$Cx
  # number of events and censorings in interval x
  DCx = Dx+Cx
  # initial cohort size
  N0 = sum(DCx)
  # adjusted population at risk at start of interval x (Berksons Formula)
  Nx = c(N0, N0 - cumsum(DCx)[-J])
  Nx = Nx - 0.5*Cx # assume censoring taking place halfway into interval x
  # adjusted number of survivors at the end of x
  Px = Nx-Dx
  # probability of dying in x given survival to x
  qx = Dx/Nx
  # probability of surviving to x+1 given survival to x
  px = 1-qx
  # probability of surviving to x (lifetable survivor function and demographic
  # variant of the Kaplan-Meier estimator)
  lx = c(1, cumprod(px))[-J]
  # standard error of surviving to x (Greenwood's formula, used for reporting)
  lx_se = c(0, lx[-1]*sqrt(cumsum(Dx/(Nx*Px))[-J]))
  # standard error of survival function adjusted to never return confidence
  # intervals outside of [0,1]; these are standard errors of log(-log(lx))
  # "Exponential Greenwood Formula" (Kalbfleisch and Prentice, 1980)
  lx_se2 = c(0, lx[-1]*sqrt(cumsum(Dx/(Nx*Px)) / (cumsum(log(Px/Nx)))^2)[-J])
  # hazard of death in interval x (different from qx only at large hazards)
  hx = qx / (1 - 0.5*qx)
  # standard error hx
  hx_se = hx*sqrt((1 - (0.5*hx)^2) / Dx)

  # confidence intervals for lx and hx
  z = -qnorm((1-alpha)/2)
  lx_upper = lx^(exp(z*lx_se2))
  lx_lower = lx^(exp(-z*lx_se2))
  hx_upper = hx + z*hx_se
  hx_lower = hx - z*hx_se

  lt <- data.frame(
    x = DxCx$time,
    Dx, Nx, Cx, lx, lx_se, lx_lower, lx_upper, hx, hx_se, hx_lower, hx_upper
  )
  return(lt)
}

# Calculate infant cohort lifetables --------------------------------------

# by date of conception
fideath %>%
  filter(type == "infant") %>%
  group_by(date_of_conception_y) %>%
  do(CohortLT(time = .$age_at_death_or_cens_d, event = .$death)) -> ilt_doc

write_csv(ilt_doc, "./out/data/lifetables/ilt_doc.csv")

# by date of conception and sex
fideath %>%
  filter(type == "infant") %>%
  group_by(date_of_conception_y, sex) %>%
  do(CohortLT(time = .$age_at_death_or_cens_d, event = .$death)) -> ilt_doc_sex

write_csv(ilt_doc_sex, "./out/data/lifetables/ilt_doc_sex.csv")

# Plot infant lifetables --------------------------------------------------

ilt_x_break = c(0, 1, 7, seq(30, 360, 30))
ilt_x_lab   = c("Day of birth", "1 Day", "1 Week",
                "1 Month", rep("", 10),
                "1 Year")
ilt_y_break = c(1e-07, 1e-06, 1e-05, 1e-04, 1e-03)

ilt_doc %>%
  filter(date_of_conception_y == 2009, hx != 0) %>%
  ggplot(aes(x)) +
  geom_ribbon(aes(ymin = hx_lower, ymax = hx_upper),
              fill = "#BDBDBD", stat = "stepribbon") +
  geom_step(aes(y = hx)) +
  scale_y_continuous("Daily hazard of death", trans = "log10",
                     breaks = c(1e-07, 1e-06, 1e-05, 1e-04, 1e-03),
                     labels = function (x) x*10000) +
  scale_x_continuous("Age", trans = "log10", breaks = ilt_x_break, labels = ilt_x_lab) +
  coord_cartesian(ylim = c(1e-07, 1e-03)) +
  ggtheme::ggtheme_min(base_family = "sans", grid = "xy") +
  theme(aspect.ratio = 0.8) -> plot_ilt_doc2009_loglog

ggsave("./out/fig/plot_ilt_doc2009_loglog.pdf", plot_ilt_doc2009_loglog, width = 7, height = 5)

# linear scale
ilt_doc %>%
  filter(date_of_conception_y == 2009, hx != 0) %>%
  ggplot(aes(x)) +
  geom_ribbon(aes(ymin = hx_lower, ymax = hx_upper),
              fill = "#BDBDBD", stat = "stepribbon") +
  geom_step(aes(y = hx)) +
  scale_y_continuous("Daily hazard of death",
                     breaks = c(0, 1e-04, 1e-03),
                     labels = function (x) x*10000) +
  scale_x_continuous("Age", breaks = seq(0, 360, 30)) +
  coord_cartesian(ylim = c(1e-07, 1e-03)) +
  ggtheme::ggtheme_min(base_family = "sans", grid = "xy") +
  theme(aspect.ratio = 0.8) -> plot_ilt_doc2009_linear

ggsave("./out/fig/plot_ilt_doc2009_linear.pdf", plot_ilt_doc2009_linear, width = 7, height = 5)
