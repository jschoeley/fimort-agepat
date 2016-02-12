# Lifetable Function ------------------------------------------------------

#' Calculate a Lifetable from a Survfit Object
#'
#' @param surfit_object object returned from the \code{\link{survfit}} function
#' @return A Lifetable with confidence interval estimates for each variable.
#' @details  The confidence intervals are derived from the \code{\link{survfit}}
#'   confidence interval calculations of the Kaplan-Meier survival curve.
SurvfitLifetable <- function (survfit_object) {

  # set up strata
  if (is.null(survfit_object$strata)) {
    strat_name   <- "strata=1"
    strat_length <- length(survfit_object$time)
  } else {
    strat_name   <- names(survfit_object$strata)
    strat_length <- survfit_object$strata
  }

  # calculate lifetable from KM-estimates given by `survfit`
  data_frame(
    strat    = rep(strat_name, strat_length),
    x        = survfit_object$time,
    Dx       = survfit_object$n.event,
    Nx       = survfit_object$n.risk,
    lx       = survfit_object$surv,
    lx_lower = survfit_object$lower,
    lx_upper = survfit_object$upper
  ) %>%
    # group by strata
    group_by(strat) %>%
    mutate(
      # survival curve
      lx       = c(1, lx[-length(lx)]),
      lx_lower = c(1, lx_lower[-length(lx_lower)]),
      lx_upper = c(1, lx_upper[-length(lx_upper)]),
      # unconditional probability of survival
      px       = lead(lx, 1) / lx,
      px_lower = lead(lx_lower, 1) / lx_lower,
      px_upper = lead(lx_upper, 1) / lx_upper,
        # if no one dies we have no information on mortality
        px       = ifelse(px       == 1, NA, px),
        px_lower = ifelse(px_lower == 1, NA, px_lower),
        px_upper = ifelse(px_upper == 1, NA, px_upper),
      # unconditional probability of event
      qx       = 1-px,
      qx_lower = 1-px_lower,
      qx_upper = 1-px_upper,
      # hazard rate
      mx       = -log(px),
      mx_lower = -log(px_lower),
      mx_upper = -log(px_upper)
    ) %>% ungroup() -> lifetable

  return(lifetable)
}
