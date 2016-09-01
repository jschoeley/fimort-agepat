library(dplyr)
library(ggplot2)

load("../../priv/data/processed_microdata/us_fideath_con_2009.Rdata")

#' This is the distribution of life-births by week of gestation for US infants conceived in 2009.

us_fideath_con_2009 %>%
  filter(type == "infant") -> lifebirth

lifebirth %>%
  count(gestation_at_delivery_w) %>%
  mutate(p = round(n / sum(n), 3)) %>% print(n = nrow(.))

lifebirth %>%
  ggplot(aes(x = gestation_at_delivery_w+1)) + # align the bins to integer boundaries
  geom_histogram(aes(y = ..density..), binwidth = 1, boundary = 1, color = "white") +
  scale_x_continuous("Week of gestation", breaks = 20:50)

lifebirth$gestation_at_delivery_w %>% quantile()
