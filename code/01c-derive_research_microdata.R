#############################
# DERIVE RESEARCH MICRODATA #
#############################

# We prepare the microdata for analysis:
# 1) generate new variables,
# 2) filter out cases we don't want for the analysis.

library(dplyr)
library(lubridate)

local({
  # us fetal- infant deaths and births individual level data
  load("./priv/data/harmonized/us_fideath_1989-2010.RData")

  fideath %>%
    filter(gestation_at_delivery_w > 22) %>%
    mutate(
      gestation_at_delivery_group =
        # discretize gestational age at delivery
        cut(gestation_at_delivery_w,
            breaks = c(23, 28, 32, 37, 39, 41, 42, 52),
            right = FALSE, include.lowest = TRUE, # actually means to include the highest
            labels = c("extremely preterm [23,28)", "very preterm [28, 32)",
                       "moderate to late preterm [32, 37)", "early term [37, 39)",
                       "full term [39, 41)", "late term [41, 42)",
                       "post term [42, 50)")),
      # construct conception cohorts
      date_of_delivery_ym =
        ymd(paste(date_of_delivery_y, date_of_delivery_m, "01", sep = "-")),
      date_of_conception_ym =
        round_date(date_of_delivery_ym - weeks(gestation_at_delivery_w), unit = "month"),
      date_of_conception_y =
        year(date_of_conception_ym),
      # death indicator
      death = !is.na(age_at_death_d),
      # age at death in (completed) weeks
      age_at_death_w = floor(age_at_death_d/7),
      # gestation at death in (completed) weeks
      gestation_at_death_w =
        ifelse(type == "infant",
               gestation_at_delivery_w + age_at_death_w,
               gestation_at_delivery_w),
      # survival time until death or censoring by gestational age in weeks
      gestation_at_death_or_cens_w =
        ifelse(death, # == TRUE
               gestation_at_death_w,
               52 + gestation_at_delivery_w),
      # (only for infants) age at death or censoring in days
      age_at_death_or_cens_d =
        ifelse(death, # == TRUE
               age_at_death_d,
               365)
    ) %>%
    # filter out incomplete conception cohorts or those with data issues
    filter(!date_of_conception_y %in% c(1988, 1991:1994, 2002:2004, 2010)) ->> fideath
})

# save the processed microdata
save(fideath, file = "./priv/data/processed_microdata/us_fideath_1989-2010.RData.RData")
