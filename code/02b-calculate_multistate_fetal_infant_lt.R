#######################################
# A MULTISTATE FETAL-INFANT LIFETABLE #
#######################################

# Init --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readr)

load("./priv/data/processed_microdata/us_fideath_1989-2010.RData")
source("./code/ggplot_stepwise_ribbon.R")

# Lifetable functions -----------------------------------------------------

#' A Lifetable Featuring Confidence Intervals for lx and hx
LT <- function (x, Dx, Nx, Cx, alpha = 0.95) {

  # number of ages
  J = length(x)

  # adjusted population at risk at start of interval x (Berksons Formula)
  Nx = Nx - 0.5*Cx
  # adjusted number of survivors at the end of x
  Px = Nx-Dx
  # adjusted probability of dying in x given survival to x
  qx = Dx/Nx
  # adjusted probability of surviving to x+1 given survival to x
  px = 1-qx
  # probability of surviving to x (lifetable survivor function and demographic
  # variant of the Kaplan-Meier estimator)
  lx = c(1, cumprod(px))[-J]
  # hazard of death in interval x (different from qx only at large hazards)
  hx = qx / (1 - 0.5*qx)

  # standard error of surviving to x (Greenwood's formula, used for reporting)
  lx_se = c(0, lx[-1]*sqrt(cumsum(Dx/(Nx*Px))[-J]))
  # standard error of survival function adjusted to never return confidence
  # intervals outside of [0,1]; these are standard errors of log(-log(lx))
  # "Exponential Greenwood Formula" (Kalbfleisch and Prentice, 1980)
  lx_se2 = c(0, lx[-1]*sqrt(cumsum(Dx/(Nx*Px)) / (cumsum(log(Px/Nx)))^2)[-J])
  # standard error hx
  hx_se = hx*sqrt((1 - (0.5*hx)^2) / Dx)

  # confidence intervals for lx and hx
  z = -qnorm((1-alpha)/2)
  lx_upper = lx^(exp(z*lx_se2))
  lx_lower = lx^(exp(-z*lx_se2))
  hx_upper = hx + z*hx_se
  hx_lower = hx - z*hx_se

  lt <-
    data.frame(
      lx, lx_se, lx_lower, lx_upper, hx, hx_se, hx_lower, hx_upper
    )

  return(lt)

}

#' A Multistate Fetal-Infant Cohort Lifetable
#'
#' @param dat A matrix of transition times for individuals with NROW = number
#'   of individuals and COLS = [fetus->infant, fetus->dead, fetus->censored,
#'   infant->dead, infant->censored]. Transition times must be given as
#'   integers. NA marks no transition.

# TODO: Lifetable closes not properly.
MuStaFICOLT <- function (dat, alpha = 0.95, shorten = TRUE) {

  # highest transition age
  max_age   = max(dat, na.rm = TRUE)
  # integer age vector
  x = 0L:max_age
  # initial cohort size
  N0 = nrow(dat)

  # table of transitions for each point in time
  trans <-
    data.frame(
      x,
      apply(dat, 2, function(x) tabulate(x+1L, nbins = max_age+1L))
    )
  names(trans) <- c("x", "fi", "fd", "fc", "id", "ic")

  # table of state occupancy for each point in time
  # state count lagged by 1 (x = 1:max_age+1)
  state_lag <-
    data.frame(
      x = x+1L,
      # fetus at risk (initial population - cumulated fetal losses)
      f = N0 - cumsum(trans$fi + trans$fd + trans$fc),
      # infants at risk
      # (cumulated births - cumulated infant deaths - cumulated infant censoring)
      i = cumsum(trans$fi) - cumsum(trans$id) - cumsum(trans$ic),
      # dead
      d = cumsum(trans$fd + trans$id),
      # censored
      c  = cumsum(trans$fc + trans$ic)
    )
  # bind state counts to initial state distribution
  states <-
    rbind(
      # first row, initial state distribution time = 0
      data.frame(x = 0L, f = N0, i = 0L, d = 0L, c = 0L),
      state_lag[-nrow(state_lag),]
    )

  # bind table of transitions and states
  lt <- cbind(states, trans[,-1])

  # lifetable for transition fetus -> infant (birth table)
  lt_fi <-
    LT(lt$x, Dx = lt$fi, Nx = lt$f, Cx = lt$fc, alpha = alpha)
  names(lt_fi) <- paste("fi", names(lt_fi), sep = "_")
  # lifetable for transition fetus -> death (fetal lifetable)
  lt_fd <-
    LT(lt$x, Dx = lt$fd, Nx = lt$f, Cx = lt$fc, alpha = alpha)
  names(lt_fd) <- paste("fd", names(lt_fd), sep = "_")
  # lifetable for transition infant -> death (infant lifetable)
  lt_id <-
    LT(lt$x, Dx = lt$id, Nx = lt$i, Cx = lt$ic, alpha = alpha)
  names(lt_id) <- paste("id", names(lt_id), sep = "_")
  # lifetable for transition fetus|infant -> death (ontogenescent lifetable)
  lt_fdid <-
    LT(lt$x, Dx = lt$id+lt$fd, Nx = lt$i+lt$f, Cx = lt$ic+lt$fc, alpha = alpha)
  names(lt_fdid) <- paste("fdid", names(lt_fdid), sep = "_")

  return(
    data.frame(lt,
               lt_fi, lt_fd, lt_id, lt_fdid,
               row.names = NULL)
  )
}

# Calculate multistate fetal-infant lifetable -----------------------------

# calculate transition times for individuals
fideath %>%
  mutate(
    # fi
    gestation_at_birth_w =
      ifelse(type == "infant",
             gestation_at_delivery_w,
             NA),
    # fd
    gestation_at_fetal_death_w =
      ifelse(type == "fetus",
             gestation_at_death_w,
             NA),
    # fc (fetus can't be censored)
    gestation_at_fetal_censoring_w = NA,
    # id
    gestation_at_infant_death_w = ifelse(type == "infant",
                                         gestation_at_death_w,
                                         NA),
    # ic (infants are censored 52 weeks after birth)
    gestation_at_infant_cens_w = ifelse(type == "infant",
                                        gestation_at_delivery_w + 52,
                                        NA)
  ) -> fideath

# calculate multistate lifetables by date of conception and sex
fideath %>%
  group_by(date_of_conception_y, sex) %>%
  do(
    MuStaFICOLT(matrix(
      c(.$gestation_at_birth_w,
        .$gestation_at_fetal_death_w,
        .$gestation_at_fetal_censoring_w,
        .$gestation_at_infant_death_w,
        .$gestation_at_infant_cens_w),
      ncol = 5))
  ) ungroup() -> filt

write_csv(filt, "./out/data/lifetables/filt_doc_sex.csv")

# Plot --------------------------------------------------------------------

# fi_hx versus density of live-births
filt %>%
  filter(date_of_conception_y == 2009, sex == "Male", x >= 23) %>%
  ggplot(aes(x)) +
  # ontogenescent mortality
  geom_ribbon(aes(ymin = fdid_hx_lower, ymax = fdid_hx_upper),
              fill = "#BDBDBD", stat = "stepribbon") +
  geom_step(aes(y = fdid_hx)) +
  geom_density(aes(x = gestation_at_delivery_w, y = ..scaled..*0.0003),
               adjust = 10, fill = "grey", colour = NA,
               data = filter(fideath,
                             sex == "Male", date_of_conception_y == 2009, type == "infant")) +
  scale_y_continuous("Weekly fetal-infant mortality rate",
                     breaks = seq(0, 1e-3, 1e-4)) +
  scale_x_continuous("Weeks of gestational age",
                     breaks = c(23, 30, 40, 50, 60 ,70, 80, 90),
                     limits = c(23, 92)) +
  ggtheme::ggtheme_min(grid = "xy", base_family = "sans")

ggsave("./doc/2016-09-01-epc2016-the_gestational_age_pattern_of_human_mortality/fig/fi_hx_births_2009_male.pdf",
       width = 5.76, height = 3.76)

# fi_hx by sex
filt %>%
  filter(date_of_conception_y == 2009, x >= 23) %>%
  ggplot(aes(x)) +
  # ontogenescent mortality
  geom_ribbon(aes(ymin = fdid_hx_lower, ymax = fdid_hx_upper, group = sex),
              fill = "#BDBDBD", stat = "stepribbon") +
  geom_step(aes(y = fdid_hx, colour = sex), show.legend = FALSE) +
  scale_y_continuous("Weekly fetal-infant mortality rate",
                     breaks = c(1e-06, 1e-05, 1e-04, 1e-03),
                     trans = "log10") +
  scale_x_continuous("Weeks of gestational age",
                     breaks = c(23, 30, 40, 50, 60 ,70, 80, 90),
                     limits = c(23, 92)) +
  scale_colour_manual(values = c(Male = "#3191C9", Female = "#D23737")) +
  ggtheme::ggtheme_min(grid = "xy", base_family = "sans")

ggsave("./doc/2016-09-01-epc2016-the_gestational_age_pattern_of_human_mortality/fig/fi_hx_2009_sex.pdf",
       width = 5.76, height = 3.76)

# fi_hx by conception cohort
filt %>%
  filter(date_of_conception_y %in% c(1989, 2009), sex == "Male", x >= 23) %>%
  ggplot(aes(x)) +
  # ontogenescent mortality
  geom_ribbon(aes(ymin = fdid_hx_lower, ymax = fdid_hx_upper, group = date_of_conception_y),
              fill = "#BDBDBD", stat = "stepribbon") +
  geom_step(aes(y = fdid_hx, group = date_of_conception_y), show.legend = FALSE) +
  scale_y_continuous("Weekly fetal-infant mortality rate",
                     breaks = c(1e-06, 1e-05, 1e-04, 1e-03),
                     trans = "log10") +
  scale_x_continuous("Weeks of gestational age",
                     breaks = c(23, 30, 40, 50, 60 ,70, 80, 90),
                     limits = c(23, 92)) +
  ggtheme::ggtheme_min(grid = "xy", base_family = "sans")

ggsave("./doc/2016-09-01-epc2016-the_gestational_age_pattern_of_human_mortality/fig/fi_hx_doc_male.pdf",
       width = 5.76, height = 3.76)

filt %>%
  filter(date_of_conception_y == 2009, sex == "Male", x %in% 23:47) %>%
  ggplot(aes(x)) +
  # CIs
  geom_ribbon(aes(y = fd_hx, ymax = fd_hx_upper, ymin = fd_hx_lower),
              fill = "#D23737", alpha = 0.3, stat = "stepribbon") +
  geom_ribbon(aes(y = id_hx, ymax = id_hx_upper, ymin = id_hx_lower),
              fill = "#3191C9", alpha = 0.3, stat = "stepribbon") +
  # fetal mortality
  geom_step(aes(y = fd_hx), colour = "#D23737")  +
  # infant mortality
  geom_step(aes(y = id_hx), colour = "#3191C9") +
  # ontogenescent mortality
  geom_step(aes(y = fdid_hx)) +
  geom_point(aes(y = fdid_hx)) +
  scale_y_continuous(breaks = c(0.0001, 0.001, 0.01, 0.1, 1), trans = "log10") +
  scale_x_continuous("Weeks of gestational age", breaks = 23:47, limits = c(23, 47)) +
  coord_cartesian(ylim = c(1e-04, 1)) +
  ggtheme::ggtheme_min(grid = "xy", base_family = "sans")

ggsave("./doc/2016-09-01-epc2016-the_gestational_age_pattern_of_human_mortality/fig/ms.pdf",
       width = 5.76, height = 3.76)

# precent born
filt %>%
  filter(date_of_conception_y == 2009, sex == "Male", x %in% 23:47) %>%
  ggplot(aes(x-0.5)) +
  geom_bar(aes(y = 1-fi_lx), fill = "grey", stat = "identity", width = 1) +
  scale_x_continuous("Weeks of gestational age", breaks = 23:47, limits = c(23, 47)) +
  scale_y_continuous(labels = c("1e+00", "1e-01", "1e-01", "1e-01", "1e-01")) +
  ggtheme::ggtheme_min(grid = "xy", base_family = "sans")

ggsave("./doc/2016-09-01-epc2016-the_gestational_age_pattern_of_human_mortality/fig/ms_pborn.pdf",
       width = 5.76, height = 3.76)
