# Survival Object ---------------------------------------------------------

# we use the survfit function to get age-specific exposures
# and deaths from the microdata in order to construct a lifetable

# infant survival, age at death
with(filter(us_fideath, type == "infant"),
     Surv(time  = age_at_death_or_cens_d,
          event = death,
          type  = "right")
) -> us_ideath_age_surv

# Estimate Kaplan Meier Survival Infant ----------------------------------

# by date of conception
us_ideath_age_survfit_doc <-
  survfit(us_ideath_age_surv ~ date_of_conception_y,
          data = filter(us_fideath, type == "infant"))

# save survival object for US infant by date of conception
# (demographic age)
save(us_ideath_age_survfit_doc,
     file = "./priv/data/us_ideath_age_survfit_doc.Rdata")
#load("./priv/data/us_ideath_age_survfit_doc.Rdata")

# by date of conception and gestational age at delivery
us_ideath_age_survfit_doc_gestdel <-
  survfit(us_ideath_age_surv ~ date_of_conception_y + gestation_at_delivery__group,
          data = filter(us_fideath, type == "infant"))

# save survival object for US infant by date of conception
# and gestational age at delivery
# (demographic age)
save(us_ideath_age_survfit_doc_gestdel,
     file = "./priv/data/us_ideath_age_survfit_doc_gestdel.Rdata")
#load("./priv/data/us_ideath_age_survfit_doc_gestdel.Rdata")

# Build Lifetables --------------------------------------------------------

# by date of conception
us_ideath_age_survfit_doc_lifetable <- SurvfitLifetable(us_ideath_age_survfit_doc)
us_ideath_age_survfit_doc_lifetable %>%
  mutate(date_of_conception_y = str_extract(strat, "(?<==).+$")) -> us_ideath_age_survfit_doc_lifetable

# save lifetable for US infant by date of conception
# (demographic age)
save(us_ideath_age_survfit_doc_lifetable,
     file = "./priv/data/us_ideath_age_survfit_doc_lifetable.Rdata")

# filter to conception cohort 2009
us_ideath_age_survfit_doc_lifetable %>%
  filter(date_of_conception_y == 2009) -> us_ideath_age_survfit_doc_2009_lifetable

# save lifetable for US infant of conception cohort 2009
# (demographic age)
save(us_ideath_age_survfit_doc_2009_lifetable,
     file = "./priv/data/us_ideath_age_survfit_doc_2009_lifetable.Rdata")

# by date of conception and gestational age at delivery
us_ideath_age_survfit_doc_gestdel_lifetable <- SurvfitLifetable(us_ideath_age_survfit_doc_gestdel)
us_ideath_age_survfit_doc_gestdel_lifetable %>%
  separate(strat, into = c("date_of_conception_y", "gestation_at_delivery_group"),
           sep = 25, remove = TRUE) %>%
  mutate(
    date_of_conception_y =
      str_extract(date_of_conception_y, "(?<==).+$"),
    gestation_at_delivery_group =
      str_extract(gestation_at_delivery_group, "(?<==).+$")
    ) -> us_ideath_age_survfit_doc_gestdel_lifetable

# save lifetable for US infant by conception cohort
# and gestational age at delivery
# (demographic age)
save(us_ideath_age_survfit_doc_gestdel_lifetable,
     file = "./priv/data/us_ideath_age_survfit_doc_gestdel_lifetable.Rdata")
