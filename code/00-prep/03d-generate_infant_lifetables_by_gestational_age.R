# Survival Object ---------------------------------------------------------

# we use the survfit function to get age-specific exposures
# and deaths from the microdata in order to construct a lifetable

us_fideath_con_2009 %>%
  mutate(gestation_at_death_interval2 = ifelse(death == TRUE,
                                               gestation_at_death_or_cens_w + 1,
                                               NA)) -> us_fideath_con_2009

# infant survival, age at death
with(filter(us_fideath_con_2009, type == "infant"),
     Surv(
       # start time is not allowed to be stop time, therefore we add
       # to the event/censoring time
       time  = gestation_at_delivery_w,
       time2 = gestation_at_death_or_cens_w+0.01,
       event = death
     )
) -> us_ideath_gestage_surv

# Estimate Kaplan Meier Survival Infant ----------------------------------

# by date of conception
us_ideath_gestage_survfit_doc <-
  survfit(us_ideath_gestage_surv ~ date_of_conception_y,
          data = filter(us_fideath_con_2009, type == "infant"))

# save survival object for US infant by date of conception
# (gestational age)
save(us_ideath_gestage_survfit_doc,
     file = "./priv/data/us_ideath_gestage_survfit_doc.Rdata")
#load("./priv/data/us_ideath_gestage_survfit_doc.Rdata")

# Build Lifetables --------------------------------------------------------

# by date of conception
us_ideath_gestage_survfit_doc_lifetable <- SurvfitLifetable(us_ideath_gestage_survfit_doc)
us_ideath_gestage_survfit_doc_lifetable %>%
  mutate(date_of_conception_y = str_extract(strat, "(?<==).+$")) -> us_ideath_gestage_survfit_doc_lifetable

# filter to conception cohort 2009
us_ideath_gestage_survfit_doc_lifetable %>%
  filter(date_of_conception_y == 2009) -> us_ideath_gestage_survfit_doc_2009_lifetable
