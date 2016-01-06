# Adaptation and Frailty Components ---------------------------------------

# Gompertz (cumulative) hazard
GHzrd <- function (x, a, b) {
  a*exp(b*x)
}
GCumHzrd <- function (x, a, b) {
  a/b * (exp(b*x) - 1)
}

# Gamma-Gompertz frailty model
GGPopHzrd <- function (x, a, b, gamma) {
  GHzrd(x, a, b) / (GCumHzrd(x, a, b)*gamma + 1)
}

# Birth Trauma Component --------------------------------------------------

BetaHzrd <- function (x, A, s1, s2, a = 0, b = 1) {
  A*dBeta_ab(x, s1, s2, a, b)
}

# Full Model --------------------------------------------------------------

TheModel <- function (x, pars) {
  a1 = pars[1]; b  = pars[2]; gamma = pars[3]
  a2 = pars[4]; s1 = pars[5]; s2    = pars[6]
  return(
    GGPopHzrd(x,
              a1, b, gamma) +
      BetaHzrd(x,
               a2, s1, s2,
               a = 23-23, b = 47-23)
  )
}

# Objective Function ------------------------------------------------------


# poisson likelihood vector
PoisML <- function (pars, age, obsDx, obsNx, HzrdFnct) {

  # predict hazard on basis of parameter estimates
  predHzrd <- HzrdFnct(x = age, pars)

  # poisson log-likelihood vector
  return(obsDx*log(predHzrd) - obsNx*predHzrd)

}
