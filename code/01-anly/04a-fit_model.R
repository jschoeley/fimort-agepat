# initialize model parameters
init_pars <- c(a1    = 0.001,  # mortality level at week 23 (process time 0)
               b     = -0.1,   # relative rate of age mortality decline (adaptation)
               gamma = 0.5,    # variance of frailties at week 0
               a2    = 0.0001, # added mortality risk due to birth
               s1    = 39-23,  # age where onset of labor is most likely
               s2    = 10)     # spread of onset of labor


# fit model
us_fideath_2009_fit <-
  maxLik(
    logLik = PoisML,
    start  = init_pars,
    method = "BFGS",
    # data
    age = us_fideath_gest_survfit_doc_2009_lifetable$x-23 ,
    obsDx = us_fideath_gest_survfit_doc_2009_lifetable$Dx,
    obsNx = us_fideath_gest_survfit_doc_2009_lifetable$Nx,
    # hazard function
    HzrdFnct = TheModel,
    # options
    iterlim = 10000
  )

# plot predicted versus observed mortality rates
plot_us_fimort_2009_mx_predobs <-
  ggplot(us_fideath_gest_survfit_doc_2009_lifetable,
         aes(x = x-23)) +
  # observed hazard
  geom_step(aes(y = mx), na.rm = TRUE) +
  # predicted hazard
  stat_function(fun = TheModel,
                arg = list(pars = us_fideath_2009_fit$estimate),
                color = rcpal$quacla[1], size = 1,
                n = 1000) +
  # scale
  scale_y_continuous("Observed weekly mortality rate\nversus predicted hazard values") +
  scale_x_continuous("Gestational Age in Weeks",
                     breaks = seq(20, 90, 10) - 23,
                     labels = seq(20, 90, 10)) +
  # annotate
  annotate(geom = "text", x = 30, y = 0.0006,
           label = paste(format(us_fideath_2009_fit$estimate,
                                scientific = TRUE, digits = 3),
                         collapse = "\n"),
           family = "serif") +
  # axis
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # theme
  ggtheme_min(grid = "xy")

ExportPDF(plot_us_fimort_2009_mx_predobs,
          path = "./fig/us_fimort_2009_mx_predobs.pdf",
          width = 15, height = 10)


