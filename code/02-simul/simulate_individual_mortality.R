a1 =  6e-04
b  = -5e-02
a2 =  1e-04

# individual hazard
h0 <- function (x, a1, b, a2, t) {
  robust <- a1 * exp(b*x)
  return(ifelse(x == t, robust + a2, robust))
}

# plot individual hazard
plot(h0(x = 20:90, a1 = a1, b = b, a2 = a2, t = 40))

foo <- h0(x = 20:90, a1 = a1, b = b, a2 = a2, t = 40)

vapply(foo, function (x) rexp(n = 1, rate = x), FUN.VALUE = c(rate = 0))

