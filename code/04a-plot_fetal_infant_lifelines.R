#############
# LIFELINES #
#############

# Init --------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)

load("./priv/data/processed_microdata/us_fideath_1989-2010.RData")

# Transform data into lifeline format -------------------------------------

# Remember, for parallel lifelines we need to include the duration information
# we have in weeks of gestational age into the dates and durations which are
# rounded to full months.

fideath %>%
  filter(date_of_conception_y == 2009) %>%
  group_by(type, death) %>%
  sample_n(10) %>%
  mutate(
    # fi
    gestation_at_birth_w = ifelse(type == "infant", gestation_at_delivery_w, NA),
    # fd
    gestation_at_fetal_death_w = ifelse(type == "fetus", gestation_at_death_w, NA),
    # fc (fetus can't be censored)
    gestation_at_fetal_censoring_w = NA,
    # id
    gestation_at_infant_death_w = ifelse(type == "infant", gestation_at_death_w, NA),
    # ic
    gestation_at_infant_cens_w = ifelse(type == "infant", gestation_at_delivery_w + 52, NA)
  ) %>%
  select(
    date_of_conception_y, date_of_conception_ym,
    sex,
    gestation_at_fetal_death_w,
    gestation_at_birth_w,
    gestation_at_infant_death_w,
    gestation_at_infant_cens_w
  ) %>%
  mutate(
    date_of_conception_ym   = date_of_conception_ym + days(as.integer(runif(n(), -10, 10))), # add jitter
    date_of_study_entry_ym  = date_of_conception_ym + weeks(23),
    date_of_fetal_death_ym  = date_of_conception_ym + weeks(gestation_at_fetal_death_w),
    date_of_birth_ym        = date_of_conception_ym + weeks(gestation_at_birth_w),
    date_of_infant_death_ym = date_of_conception_ym + weeks(gestation_at_infant_death_w),
    date_of_infant_cens_ym  = date_of_conception_ym + weeks(gestation_at_infant_cens_w)
  ) -> lifelines


# Plot fetal-infant lifelines ---------------------------------------------

lifelines %>%
  ggplot() +
  # Lexis grid
  geom_hline(yintercept = c(0, 52),
             colour = "grey") +
  geom_vline(xintercept = as.numeric(date(c("2009-01-01", "2010-01-01", "2011-01-01"))),
             colour = "grey") +
  # survival to study entry
  geom_segment(aes(x = date_of_conception_ym,
                   xend = date_of_study_entry_ym,
                   y = 0,
                   yend = 23),
               lty = "dotted", colour = "grey") +
  # period study entry -> fetal death
  geom_segment(aes(x = date_of_study_entry_ym,
                   xend = date_of_fetal_death_ym,
                   y = 23,
                   yend = gestation_at_fetal_death_w),
               colour = rcpal::rcpal$quacla[1]) +
  # period study entry -> birth
  geom_segment(aes(x = date_of_study_entry_ym,
                   xend = date_of_birth_ym,
                   y = 23,
                   yend = gestation_at_birth_w),
               colour = rcpal::rcpal$quacla[1]) +
  # period birth -> infant death
  geom_segment(aes(x = date_of_birth_ym,
                   xend = date_of_infant_death_ym,
                   y = gestation_at_birth_w,
                   yend = gestation_at_infant_death_w),
               colour = rcpal::rcpal$quacla[2]) +
  # period birth -> infant censoring
  geom_segment(aes(x = date_of_birth_ym,
                   xend = date_of_infant_cens_ym,
                   y = gestation_at_birth_w,
                   yend = gestation_at_infant_cens_w),
               colour = rcpal::rcpal$quacla[2]) +
  # fetal death
  geom_point(aes(x = date_of_fetal_death_ym,
                 y = gestation_at_fetal_death_w),
             shape = 3, colour = rcpal::rcpal$quacla[1]) +
  # birth
  geom_point(aes(x = date_of_birth_ym,
                 y = gestation_at_birth_w),
             colour = rcpal::rcpal$quacla[2]) +
  # infant death
  geom_point(aes(x = date_of_infant_death_ym,
                 y = gestation_at_infant_death_w),
             shape = 3, colour = rcpal::rcpal$quacla[2]) +
  # infant censoring
  geom_point(aes(x = date_of_infant_cens_ym,
                 y = gestation_at_infant_cens_w),
             shape = 21, fill = "white", colour = rcpal::rcpal$quacla[2]) +
  scale_y_continuous("Gestational age in weeks", breaks = c(23, 40, 52)) +
  scale_x_date("Year", date_breaks = "1 year", date_labels = "%Y") +
  coord_equal(6.5) +
  theme_classic() +
  theme(axis.ticks = element_blank()) -> plot_lifelines

ggsave("./out/fig/lifelines.pdf", plot_lifelines)
