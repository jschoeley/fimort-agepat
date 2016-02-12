# Input -------------------------------------------------------------------

# load microdata on births, infant, and fetal deaths
load("./priv/data/us_birth_ideath_1989-2010.Rdata")
load("./priv/data/us_fdeath_1982-2013.Rdata")

# Merge Data --------------------------------------------------------------

# merge births, infant- and fetal deaths
us_fideath <- bind_rows(mutate(us_ideath, type = "infant"),
                        mutate(us_fdeath, type = "fetal"))
rm(us_ideath, us_fdeath)

# Construct Variables -----------------------------------------------------

# contruct survival analysis variables for microdata
us_fideath %>%
  filter(
    # don't include years where no cohort births / infant deaths are given
    !(date_of_delivery_y %in% c(1982:1988, 1992:1994, 2011:2013)),
    # don't include fetal death before gestational week 23
    gestation_at_delivery_w >= 23
  ) %>%
  mutate(
    # discretize gestational age at delivery
    gestation_at_delivery__group = cut(gestation_at_delivery_w,
                                       breaks = c(23, 30, 37, 39, 42, 52),
                                       right = FALSE,
                                       # actually means to include the highest
                                       include.lowest = TRUE),
    # generate delivery date
    date_of_delivery_ym =
      ymd(paste0(date_of_delivery_y, date_of_delivery_m), truncated = 2),
    # compute conception date
    date_of_conception_ym =
      round_date(date_of_delivery_ym - dweeks(gestation_at_delivery_w), unit = "month"),
    date_of_conception_y  =
      year(date_of_conception_ym),
    # age at death in weeks
    age_at_death_w = round(age_at_death_d/7),
    # compute gestational age at death
    # for fetal deaths the gestational age at death is gestational age at
    # delivery (for infant deaths plus age at death)
    gestation_at_death_w =
      ifelse(type == "infant",
             gestation_at_delivery_w + age_at_death_w,
             gestation_at_delivery_w),
    # death indicator
    death =
      ifelse(type == "fetal" | !is.na(age_at_death_d), TRUE, FALSE),
    # survival time until death or censoring by gestational age in weeks
    gestation_at_death_or_cens_w =
      ifelse(death == FALSE,
             52 + gestation_at_delivery_w,
             gestation_at_death_w),
    # survival time until death or censoring by age in days (infants only)
    age_at_death_or_cens_d =
      ifelse(death == FALSE,
             366,
             age_at_death_d)
  ) %>%
  filter(
    # don't include years with incomplete informations about conceptions
    !(date_of_conception_y %in% c(1988, 1991, 1995, 2010))
  ) -> us_fideath

# save individual level data on us fetal and infant survival
save(us_fideath, file = "./priv/data/us_fideath.Rdata")
#load("./priv/data/us_fideath.Rdata")

# subset conception cohort year 2009
us_fideath %>% filter(date_of_conception_y == 2009) -> us_fideath_con_2009

# save individual level data on us fetal and infant survival
# for conception cohort 2009
save(us_fideath_con_2009, file = "./priv/data/us_fideath_con_2009.Rdata")
#load("./priv/data/us_fideath_con_2009.Rdata")
