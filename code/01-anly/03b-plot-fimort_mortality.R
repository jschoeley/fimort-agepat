# Gestational Age Hazard 2009 ---------------------------------------------

# get age region of 95% live-births in 2009
us_fideath_birthquant_doc_2009 <-
  data_frame(y    = 10^-5,
             x    = median(us_fideath_con_2009$gestation_at_delivery_w),
             xmin = quantile(us_fideath_con_2009$gestation_at_delivery_w, probs = 0.025),
             xmax = quantile(us_fideath_con_2009$gestation_at_delivery_w, probs = 0.975))

# plot hazard across gestational age
y_scale <- GenerateScale(breaks = seq(0, 7E-4, 1E-4), sparse = FALSE)
plot_us_fimort_2009_mx <-
  ggplot(us_fideath_gest_survfit_doc_2009_lifetable) +
  # 90 % birth region
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 7.02E-04),
            alpha = 0.2,
            data = us_fideath_birthquant_doc_2009) +
  annotate("text",
           x = 38, y = 0.00005,
           label = "95 % of\nlive-births",
           size = 4.5) +
  # axis
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 20) +
  # hazard
  geom_step(aes(x = x, y = mx)) +
  scale_y_continuous("Weekly Mortality Rate m(x)",
                     breaks = y_scale$breaks, labels = y_scale$labels,
                     expand = c(0, 0)) +
  scale_x_continuous("Gestational Age x in Weeks",
                     breaks = seq(20, 90, 10),
                     expand = c(0.01, 0)) +
  ggtheme_min(grid = "xy", base_family = "sans")

ExportPDF(plot_us_fimort_2009_mx, .path = "./fig/us_fimort_2009_mx.pdf",
          .width = 20, .height = 16)

# Gestatational Age Hazard All Cohorts ------------------------------------

us_fideath_gest_survfit_doc_lifetable %>%
  mutate(highlight = ifelse(date_of_conception_y %in% c(1989, 1999, 2009),
                            "1",
                            "0")) -> us_fideath_gest_survfit_doc_lifetable

ggplot(us_fideath_gest_survfit_doc_lifetable,
       aes(group  = date_of_conception_y, x = x, y = mx, colour = highlight)) +
  geom_line(alpha = 0.5) +
  scale_colour_manual(values = c("grey", "red")) +
  scale_y_log10() +
  ggtheme_min()
