# Survival Object ---------------------------------------------------------

# we use the survfit function to get age-specific exposures
# and deaths from the microdata in order to construct a lifetable

# fetal survival, gestation at death
with(filter(us_fideath, type == "fetal"),
     Surv(time  = gestation_at_death_or_cens_w,
          event = death,
          type  = "right")
) -> us_fdeath_gest_surv

# Estimate Kaplan Meier Survival Fetal ------------------------------------

# by date of conception
us_fdeath_age_survfit_doc <-
  survfit(us_fdeath_gest_surv ~ date_of_conception_y,
          data = filter(us_fideath, type == "fetal"))

# save survival object for US fetus by date of conception
# (gestational age)
save(us_fdeath_age_survfit_doc,
     file = "./priv/data/us_fdeath_age_survfit_doc.Rdata")
