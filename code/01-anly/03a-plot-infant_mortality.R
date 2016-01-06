# Infant Hazard 2009 ------------------------------------------------------

# plot the mortality rates over age
y_scale <- GenerateYScale(y_data = us_ideath_age_survfit_doc_2009_lifetable$mx,
                          y_break_mid = seq(1E-4, 1E-3, 1E-4))
plot_us_imort_2009_mx <-
  ggplot(na.omit(us_ideath_age_survfit_doc_2009_lifetable),
       aes(x = x, y = mx)) +
  geom_step() +
  geom_rangeframe() +
  scale_x_continuous(name = "Age x Since Birth in Days",
                     breaks = seq(0, 360, 30)) +
  scale_y_continuous(name = "Daily Mortality Rate m(x)",
                     breaks = y_scale$breaks, labels = y_scale$labels) +
  ggtheme_min(grid = "xy", base_size = 17)

ExportPDF(plot_us_imort_2009_mx, .path = "./fig/us_imort_2009_mx.pdf",
          .width = 20, .height = 16)

# plot the log-mortality rates over age
y_scale <- GenerateYScale(y_data = us_ideath_age_survfit_doc_2009_lifetable$mx,
                          y_break_mid = c(seq(1E-6, 9E-6, 1E-6),
                                          seq(1E-5, 9E-5, 1E-5),
                                          seq(1E-4, 9E-4, 1E-4),
                                          1E-3))

plot_us_imort_2009_log_mx <-
  plot_us_imort_2009_mx +
  scale_y_continuous(name = "Daily Mortality Rate m(x)",
                     breaks = y_scale$breaks, labels = y_scale$labels,
                     trans = "log10")

ExportPDF(plot_us_imort_2009_log_mx, .path = "./fig/us_imort_2009_log_mx.pdf",
          .width = 20, .height = 16)
