#' Power Curves varying delta
#'
#' The data for this function are generated where the standardized \eqn{\theta} is from -3 to 3, so
#' \eqn{\delta} values, values for the width of the indifference zone, should be certainly be smaller.
#' @param n Numeric. The sample size
#' @param delta Numeric vector. The width of the indifference zone in SD units. Should be between
#'  -3 and 3, as this is the range of the \eqn{\theta} space (and likely much smaller).
#' @param truth Character. True data generating mechanism - must be one of the following:
#'   * null
#'   * alternative
#'   * inconclusive
#'
#' @return A plot of the power curves
#' @export
#'
#' @examples
#' ## Figure S3 from the second generation P-value paper. The relationship between
#' ## the probability that p_delta = 0 varying delta.
#'
#' power_curves(n = 10, delta = c(0, 1/30, 1/2, 1), truth = "null")
#'
#' ## Figure S5 from the second generation P-value paper. The relationship
#' ## between probability of data supported compatibility 517 with the null hypothesis,
#' ## and various deltas
#'
#' ## n = 40 (Figure S5 left)
#' power_curves(n = 40, delta = c(0, 1/30, 1/2, 1), truth = "alternative")
#' ## n = 200 (Figure S5 right)
#' power_curves(n = 200, delta = c(0, 1/30, 1/2, 1), truth = "alternative")
#'
#' ## Figure S6 from the second generation P-value paper. The relationship between
#' ## the probability of an inconclusive result and various deltas.
#'
#' ## n = 20 (Figure S6 left)
#' power_curves(n = 20, delta = c(0, 1/30, 1/2, 1), truth = "inconclusive")
#' ## n = 200 (Figure S6 right)
#' power_curves(n = 200, delta = c(0, 1/30, 1/2, 1), truth = "inconclusive")
power_curves <- function(n, delta, truth = "null") {

  if (!(truth %in% c("null", "alternative", "inconclusive"))) {
    stop("Parameter `truth` must be one of the following: \n  * null\n  * alternative\n  * inconclusive")
  }

  theta <- seq(-3, 3, 0.001)
  if (truth == "null") {
    p <- purrr::map(delta, power_null, n = n, theta = theta)
    plot(theta,
         p[[1]],
         type = "n",
         ylim = c(0, 1),
         xlim = c(-max(delta) - 2, max(delta) + 2),
         xlab = "Alternative (SD units)",
         ylab="Probability")
    for (i in seq_along(p)) {
      lines(theta, p[[i]], lty = 1, lwd = 1, col = i)
    }
    abline(h = 0.05, lwd = 0.5, lty = 2)
  }

  if (truth == "alternative") {
    p <- purrr::map(delta, power_alt, n = n, theta = theta)
    plot(theta,
         p[[1]],
         type = "n",
         ylim = c(0, 1),
         xlim = c(-max(delta) - 2, max(delta) + 2),
         xlab = "Alternative (SD units)",
         ylab = "Probability")
    for (i in seq_along(p)) {
      lines(theta, p[[i]], lty = 1, lwd = 1, col = i)
    }
    abline(h = 0.05, lty = 2)
  }

  if (truth == "inconclusive") {
    p_weak <- purrr::map(delta, power_inconclusive, n = n, theta = theta)

    par(yaxs = "i")

    plot(theta,
         p_weak[[1]],
         type = "n",
         ylim = c(0, length(p_weak)),
         xlim = c(-max(delta) - 2, max(delta) + 2),
         xlab = "Alternative (SD units)",
         ylab = "Probability",
         yaxt = "n")

    axis(2,
         at = seq(0, length(p_weak), by = .25),
         label = c(0, rep(c("", .5, "", 0), length(p_weak))),
         las=2)

    abline(h = 0, lty = 2, lwd = 0.5)
    for (i in seq_along(p_weak)) {
      rect(-delta[i],
           (length(p_weak) - i + 1),
           delta[i],
           length(p_weak) - i,
           col = rgb(1,0.9,1,0.6),
           border = F)
      abline(h = i, lty = 2, lwd = 0.5)
      lines(theta, p_weak[[i]] + (length(p_weak) - i), lty = 1, lwd = 1, col = i)
      text(1.6, length(p_weak) - i + 0.5, bquote(delta~"="~.(round(delta[i], 2))))
    }
  }
}

## Power under the null
power_null <- function(delta, n, theta) {
  pnorm(sqrt(n) * (-delta) - sqrt(n) * theta - 1.96) +
    pnorm(-sqrt(n) * delta + sqrt(n) * theta - 1.96)
}

## Power under the alternative
power_alt <- function(delta, n, theta) {
  p <- pnorm(sqrt(n) * delta - sqrt(n) * theta - 1.96) -
    pnorm(sqrt(n) * (-delta) - sqrt(n) * theta + 1.96)

  if (delta <= 1.96 * sqrt(1 / n)) {
    p <- rep(0, length(theta))
  }

  p
}

## Power when inconclusive
power_inconclusive <- function(delta, n, theta) {
  1 - power_null(delta, n, theta) - power_alt(delta, n, theta)
}
