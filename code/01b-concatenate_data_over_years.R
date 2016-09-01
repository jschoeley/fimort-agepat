##############################################################
# CONCATENATE US BIRTHS, INFANT- AND FETAL DEATHS OVER YEARS #
##############################################################

# We merge the data on births, infant- and fetal deaths from various years into
# a single table. This of course requires the variables to be harmonized across
# the years. Non-harmonized variables should be stored under different names,
# e.g. plurality91, plurality94. They can be harmonized after concatenantion.
# The matchin of variables across years is based on the variable names. If a
# variable is available in year y, but not in year z, then it will contain NAs
# for year z.

library(dplyr)

local({
  # load fetal deaths file, load linked birth-infant death file
  load("./priv/data/pre-harmonized/us_fdeath_1989-2010.RData")
  load("./priv/data/pre-harmonized/us_ideath_1989-2010.RData")

  # merge data on births, fetal- and infant deaths cross years
  fideath <<- bind_rows(infant = bind_rows(ideath),
                        fetus = bind_rows(fdeath),
                        .id = "type")
})

save(fideath, file = "./priv/data/harmonized/us_fideath_1989-2010.RData")
