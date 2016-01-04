# Lifetable Function ------------------------------------------------------

#' Extract a Lifetable from a Survfit Object
SurvfitLifetable <- function (survfit_object) {
  if (is.null(survfit_object$strata)) {
    strat_name   <- "strata=1"
    strat_length <- length(survfit_object$time)
  } else {
    strat_name   <- names(survfit_object$strata)
    strat_length <- survfit_object$strata
  }
  data_frame(
    strat    = rep(strat_name, strat_length),
    x        = survfit_object$time,
    Dx       = survfit_object$n.event,
    Nx       = survfit_object$n.risk,
    lx       = survfit_object$surv,
    lx_lower = survfit_object$lower,
    lx_upper = survfit_object$upper
  ) %>%
    group_by(strat) %>%
    mutate(
      lx       = c(1, lx[-length(lx)]),
      lx_lower = c(1, lx_lower[-length(lx_lower)]),
      lx_upper = c(1, lx_upper[-length(lx_upper)]),
      px       = lead(lx, 1) / lx,
      px_lower = lead(lx_lower, 1) / lx_lower,
      px_upper = lead(lx_upper, 1) / lx_upper,
      # if no one dies we have no information on mortality
      px       = ifelse(px       == 1, NA, px),
      px_lower = ifelse(px_lower == 1, NA, px_lower),
      px_upper = ifelse(px_upper == 1, NA, px_upper),
      qx       = 1-px,
      qx_lower = 1-px_lower,
      qx_upper = 1-px_upper,
      mx       = -log(px),
      mx_lower = -log(px_lower),
      mx_upper = -log(px_upper)
    ) %>% ungroup()
}
