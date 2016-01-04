# initialize model parameters
init_pars <- c(a1 = 0.001,  # mortality level at week 23 (process time 0)
               b = -0.1,    # relative rate of age mortality decline (adaptation)
               gamma = 0.5, # variance of frailties at week 0
               a2 = 0.0001, # added mortality risk due to birth
               s1 = 39-23,  # age where onset of labor is most likely
               s2 = 10)     # spread of onset of labor


# fit model
us_fideath_2009_fit <-
  maxLik(logLik = PoisML,
         start = init_pars,
         method = "BHHH",
         # data
         age = us_fideath_gest_survfit_doc_2009_lifetable$x-23 ,
         obsDx = us_fideath_gest_survfit_doc_2009_lifetable$Dx,
         obsNx = us_fideath_gest_survfit_doc_2009_lifetable$Nx,
         # hazard functioin
         HzrdFnct = TheModel,
         # options
         iterlim = 10000)

# plot predicted versus observed mortality rates
y_scale <- GenerateYScale(us_fideath_gest_survfit_doc_2009_lifetable$mx,
                          y_break_mid = seq(1E-4, 6E-4, 1E-4), sparse = FALSE)
plot_us_fimort_2009_mx_predobs <-
  ggplot(us_fideath_gest_survfit_doc_2009_lifetable,
         aes(x = x-23)) +
  geom_step(aes(y = mx)) +
  stat_function(fun = TheModel,
                arg = list(pars = us_fideath_2009_fit$estimate),
                color = rcpal$quacla[1], size = 1,
                n = 1000) +
  annotate(geom = "text", x = 30, y = 0.0006,
           label = paste(format(us_fideath_2009_fit$estimate,
                                scientific = TRUE, digits = 3),
                         collapse = "\n"),
           family = "serif") +
  scale_y_continuous("Weekly Mortality Rate m(x) versus predicted hazard mu(x)",
                     breaks = y_scale$breaks, labels = y_scale$labels) +
  scale_x_continuous("Gestational Age x in Weeks",
                     breaks = c(23, seq(30, 100, 10)) - 23,
                     labels = c(23, seq(30, 100, 10))) +
  geom_rangeframe(aes(x = x-23, y = mx),
                  data = na.omit(us_fideath_gest_survfit_doc_2009_lifetable)) +
  ggtheme_min(grid = "xy")

ExportPDF(plot_us_fimort_2009_mx_predobs,
          .path = "./fig/us_fimort_2009_mx_predobs.pdf",
          .width = 25, .height = 16)


