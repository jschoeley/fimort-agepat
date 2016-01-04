# Survival Object ---------------------------------------------------------

# we use the survfit function to get age-specific exposures
# and deaths from the microdata in order to construct a lifetable

# fetal and infant survival, gestation at death
Surv(time  = us_fideath$gestation_at_death_or_cens_w,
     event = us_fideath$death,
     type  = "right"
) -> us_fideath_gest_surv

# Estimate Kaplan Meier Survival Fetal & Infant ---------------------------

# by date of conception
us_fideath_gest_survfit_doc <-
  survfit(us_fideath_gest_surv ~ date_of_conception_y,
          data = us_fideath)

# save survival object for US fetus and infants by date of conception
# (gestational age)
save(us_fideath_gest_survfit_doc,
     file = "./priv/data/us_fideath_gest_survfit_doc.Rdata")
#load(file = "./priv/data/us_fideath_gest_survfit_doc.Rdata")

# Build Lifetables --------------------------------------------------------

# by date of conception
us_fideath_gest_survfit_doc_lifetable <- SurvfitLifetable(us_fideath_gest_survfit_doc)
us_fideath_gest_survfit_doc_lifetable %>%
  mutate(date_of_conception_y = str_extract(strat, "(?<==).+$")) -> us_fideath_gest_survfit_doc_lifetable

# save lifetable for US fetal and infant by date of conception
# (gestational age)
save(us_fideath_gest_survfit_doc_lifetable,
     file = "./priv/data/us_fideath_gest_survfit_doc_lifetable.Rdata")

# get conception cohort 2009
us_fideath_gest_survfit_doc_lifetable %>%
  filter(date_of_conception_y == 2009) -> us_fideath_gest_survfit_doc_2009_lifetable

# save lifetable for US fetal and infant of conception cohort 2009
# (gestational age)
save(us_fideath_gest_survfit_doc_2009_lifetable,
     file = "./priv/data/us_fideath_gest_survfit_doc_2009_lifetable.Rdata")
