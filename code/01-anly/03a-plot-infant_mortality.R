# Infants 2009 Lifetable --------------------------------------------------

# plot the mortality rates over age
mx_at_birth_2009 <- max(us_ideath_age_survfit_doc_2009_lifetable$mx, na.rm = TRUE)
plot_us_imort_2009_log_mx <-
  ggplot(na.omit(us_ideath_age_survfit_doc_2009_lifetable),
         aes(x = x, y = mx)) +
  # hazard function
  geom_step() +
  # scales
  scale_x_continuous(name   = "Age since birth in days",
                     breaks = seq(0, 360, 30),
                     expand = c(0.01, 0)) +
  scale_y_continuous(name   = "Daily mortality rate",
                     breaks = log_breaks(n = 10),
                     trans  = "log10",
                     expand = c(0.1, 0)) +
  # axis
  geom_vline(xintercept = 0) +
  # annotation
  annotate("point", x = 0, y = mx_at_birth_2009,
           shape = 21, size = 3, fill = rcpal$quacla[1]) +
  annotate("text", x = 3, y = mx_at_birth_2009, hjust = "left", vjust = "top",
           label = "Day of birth\nD = 4,768; N = 4,001,522\n~119 deaths per 100,000 live-births",
           family = "serif", size = 4) +
  annotate("point", x = 364, y = 7.532764e-07,
           shape = 21, size = 3, fill = rcpal$quacla[1]) +
  annotate("text", x = 360, y = 7.532764e-07, hjust = "right", vjust = "top",
           label = "Day 364\nD = 3; N = 3,982,602\n~0.075 deaths per 100,000 survivors",
           family = "serif", size = 4) +
  # theme
  ggtheme_min(grid = "xy", base_size = 17)

ExportPDF(plot_us_imort_2009_log_mx, path = "./fig/us_imort_2009_log_mx.pdf",
          width = 15, height = 12)

# plot cumulative age distribution of infant deaths
us_imort_cumdx_2009 <- with(us_ideath_age_survfit_doc_2009_lifetable,
                            cumsum(c(0,Dx[-length(Dx)]))/sum(Dx))
plot_us_imort_2009_cumdx <-
  ggplot(na.omit(us_ideath_age_survfit_doc_2009_lifetable),
         aes(x = x, y = cumsum(c(0,Dx[-length(Dx)]))/sum(Dx))) +
  # cumulative distribution function
  geom_step() +
  # scales
  scale_x_continuous(name   = "Age since birth in days",
                     breaks = seq(0, 360, 30),
                     expand = c(0.01, 0)) +
  scale_y_continuous(name   = "Probability to die prior to age x\nfor those who die in infancy",
                     labels = StripLeadingZeroInteger,
                     expand = c(0.01, 0)) +
  # axis
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # annotation
  annotate("point", x = 17, y = 0.5,
           shape = 21, size = 3, fill = rcpal$quacla[1]) +
  annotate("text", x = 22, y = 0.5, hjust = "left", vjust = "top",
           label = "~50% of deaths occour\nprior to day 17",
           family = "serif", size = 4) +
  annotate("point", x = 1, y = 0.251,
           shape = 21, size = 3, fill = rcpal$quacla[1]) +
  annotate("text", x = 5, y = 0.251, hjust = "left", vjust = "top",
           label = "25.1% of deaths occour\nat day of birth",
           family = "serif", size = 4) +
  # theme
  ggtheme_min(grid = "xy", base_size = 17)

ExportPDF(plot_us_imort_2009_cumdx, path = "./fig/us_imort_2009_cumdx.pdf",
          width = 15, height = 12)
