# Load Lifetables ---------------------------------------------------------

# load lifetable for US infant of conception cohort 2009
# (demographic age)
load(file = "./priv/data/us_ideath_age_survfit_doc_2009_lifetable.Rdata")

# load lifetable for US fetal and infant of conception cohort 2009
# (gestational age)
load(file = "./priv/data/us_fideath_gest_survfit_doc_2009_lifetable.Rdata")

# Load Microdata ----------------------------------------------------------

# load individual level data on us fetal and infant survival
# for conception cohort 2009
load("./priv/data/us_fideath_con_2009.Rdata")
