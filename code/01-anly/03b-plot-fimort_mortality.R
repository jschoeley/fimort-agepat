# Gestational Age Hazard 2009 ---------------------------------------------

# get age region of 95% live-births in 2009
us_fideath_birthquant_doc_2009 <-
  data_frame(y    = 10^-5,
             x    = median(us_fideath_con_2009$gestation_at_delivery_w),
             xmin = quantile(us_fideath_con_2009$gestation_at_delivery_w, probs = 0.025),
             xmax = quantile(us_fideath_con_2009$gestation_at_delivery_w, probs = 0.975))

# plot hazard across gestational age
plot_us_fimort_2009_mx <-
  ggplot(us_fideath_gest_survfit_doc_2009_lifetable) +
  # axis
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 20) +
  # annotate
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 7.02E-04),
            alpha = 0.2,
            data = us_fideath_birthquant_doc_2009) +
  annotate("text",
           x = 38, y = 0.00005,
           label = "95 % of\nlive-births",
           size = 4.5, family = "serif") +
  # hazard
  geom_step(aes(x = x, y = mx),  na.rm = TRUE) +
  # scale
  scale_y_continuous("Weekly mortality rate",
                     breaks = 0:7*10^(-4),
                     expand = c(0.01, 0)) +
  scale_x_continuous("Gestational age in weeks",
                     breaks = seq(20, 90, 10),
                     expand = c(0.01, 0)) +
  # theme
  ggtheme_min(grid = "xy")

ExportPDF(plot_us_fimort_2009_mx, path = "./fig/us_fimort_2009_mx.pdf",
          width = 15, height = 10)

plot_us_fimort_2009_mx +
  scale_y_log10(
    name = "m(x)",
    breaks = mutate(expand.grid(base = 1:5, exp = -5:-1),
                    breaks = base*10^exp)$breaks) -> plot_us_fimort_2009_log_mx

ExportPDF(plot_us_fimort_2009_log_mx, path = "./fig/us_fimort_2009_log_mx.pdf",
          width = 15, height = 10)

# Gestatational Age Hazard All Cohorts ------------------------------------

# us_fideath_gest_survfit_doc_lifetable %>%
#   mutate(highlight = ifelse(date_of_conception_y %in% c(1989, 1999, 2009),
#                             "1",
#                             "0")) -> us_fideath_gest_survfit_doc_lifetable
#
# ggplot(us_fideath_gest_survfit_doc_lifetable,
#        aes(group  = date_of_conception_y, x = x, y = mx, colour = highlight)) +
#   geom_line(alpha = 0.5) +
#   scale_colour_manual(values = c("grey", "red")) +
#   scale_y_log10() +
#   ggtheme_min()
