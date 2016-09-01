#####################################
# ADDING STEPWISE RIBBONS TO GGPLOT #
#####################################

# from https://groups.google.com/d/msg/ggplot2/9cFWHaH1CPs/HQRgIIk8CAAJ
stairstepn <- function(data, direction = "hv", yvars = "y") {

  direction = match.arg(direction, c( "hv", "vh"))
  data = as.data.frame(data)[order(data$x),]
  n = nrow(data)

  if (direction == "vh") {
    xs = rep(1:n, each = 2)[-2*n]
    ys = c(1,rep(2:n, each = 2))
  } else {
    ys = rep(1:n, each = 2)[-2*n]
    xs = c(1, rep(2:n, each = 2))
  }

  data.frame(
    x = data$x[xs],
    data[ys, yvars, drop = FALSE],
    data[xs, setdiff(names(data), c("x", yvars)), drop = FALSE]
  )
}

stat_stepribbon <-
  function(mapping = NULL, data = NULL, geom = "ribbon", position = "identity", inherit.aes = TRUE) {
    ggplot2::layer(
      stat = Stepribbon, mapping = mapping, data = data, geom = geom,
      position = position, inherit.aes = inherit.aes
    )
  }

StatStepribbon <-
  ggproto("stepribbon", Stat,
          compute_group = function(., data, scales, direction = "hv", yvars = c( "ymin", "ymax" ), ...) {
            stairstepn(data = data, direction = direction, yvars = yvars)
          },
          required_aes = c( "x", "ymin", "ymax" )
  )
