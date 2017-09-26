#' Power Curves varying delta
#'
#' @param delta Numeric vector. The width of the indifference zone in SD units.
#' @param theta Numeric vector. The "true" effect size (i.e., the default is `seq(-3, 3, 0.001`).
#' @param n Numeric. The sample size.
#' @param prob Character. The probability of interest (i.e., if you are interested in the probability that
#'   \eqn{p_\delta = 0}{p\delta = 0} use `null`). Must be one of the following:
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
#' power_curves(delta = c(0, 1/30, 1/2, 1), n = 10, prob = "null")
#'
#' ## Figure S5 from the second generation P-value paper. The relationship
#' ## between probability of data supported compatibility with the null hypothesis,
#' ## and various deltas
#'
#' ## n = 40 (Figure S5 left)
#' power_curves(delta = c(0, 1/30, 1/2, 1), n = 40, prob = "alternative")
#' ## n = 200 (Figure S5 right)
#' power_curves(delta = c(0, 1/30, 1/2, 1), n = 200, prob = "alternative")
#'
#' ## Figure S6 from the second generation P-value paper. The relationship between
#' ## the probability of an inconclusive result and various deltas.
#'
#' ## n = 20 (Figure S6 left)
#' power_curves(delta = c(0, 1/30, 1/2, 1), n = 20, prob = "inconclusive")
#' ## n = 200 (Figure S6 right)
#' power_curves(delta = c(0, 1/30, 1/2, 1), n = 200, prob = "inconclusive")
power_curves <- function(delta, n, theta = seq(-3, 3, 0.001), prob = "null") {

  if (!(prob %in% c("null", "alternative", "inconclusive"))) {
    stop("Parameter `truth` must be one of the following: \n  * null\n  * alternative\n  * inconclusive")
  }

  if (prob == "null") {
    p <- power_null(delta = delta, n = n, theta = theta)
    graphics::plot(theta,
                   p[p$delta == delta[1], "p"],
                   type = "n",
                   ylim = c(0, 1),
                   xlim = range(theta),
                   xlab = "Alternative (SD units)",
                   ylab="Probability")
    for (i in 1:length(delta)) {
      graphics::lines(theta, p[p$delta == delta[i], "p"], lty = 1, lwd = 1, col = i)
    }
    graphics::abline(h = 0.05, lwd = 0.5, lty = 2)
  }

  if (prob == "alternative") {
    p <- power_alt(delta = delta, n = n, theta = theta)
    graphics::plot(theta,
                   p[p$delta == delta[1], "p"],
                   type = "n",
                   ylim = c(0, 1),
                   xlim = range(theta),
                   xlab = "Alternative (SD units)",
                   ylab = "Probability")
    for (i in 1:length(delta)) {
      graphics::lines(theta, p[p$delta == delta[i], "p"], lty = 1, lwd = 1, col = i)
    }
    graphics::abline(h = 0.05, lty = 2)
  }

  if (prob == "inconclusive") {
    p <- power_inconclusive(delta = delta, n = n, theta = theta)

    graphics::par(yaxs = "i")

    graphics::plot(theta,
                   p[delta == p$delta[1], "p"],
                   type = "n",
                   ylim = c(0, length(delta)),
                   xlim = range(theta),
                   xlab = "Alternative (SD units)",
                   ylab = "Probability",
                   yaxt = "n")

    graphics::axis(2,
                   at = seq(0, length(delta), by = .25),
                   label = c(0, rep(c("", .5, "", 0), length(delta))),
                   las=2)

    graphics::abline(h = 0, lty = 2, lwd = 0.5)
    for (i in 1:length(delta)) {
      graphics::rect(-delta[i],
                     (length(delta) - i + 1),
                     delta[i],
                     length(delta) - i,
                     col = grDevices::rgb(1,0.9,1,0.6),
                     border = F)
      graphics::abline(h = i, lty = 2, lwd = 0.5)
      graphics::lines(theta, p[p$delta == delta[i], "p"] + (length(delta) - i), lty = 1, lwd = 1, col = i)
      graphics::text(1.6, length(delta) - i + 0.5, bquote(delta~"="~.(round(delta[i], 2))))
    }
  }
}

#' Probability that \eqn{p_\delta = 0}{p\delta = 0}
#'
#' @param delta Numeric vector. The width of the indifference zone in SD units.
#' @param n Numeric. The sample size.
#' @param theta The "true" effect size.
#'
#' @return Data frame with the following columns:
#'   * delta: The width of the simulated indifference zone
#'   * n: The sample size
#'   * theta: the "true" effect size
#'   * p: The probability \eqn{p_\delta = 0}{p\delta = 0}
#' @export
#'
#' @examples
#' power_null(
#'    delta = c(0, 1/30, 1/2, 1),
#'    n = 33,
#'    theta = seq(-3, 3, 0.01)
#'    )
#'
power_null <- function(delta, n, theta) {
  if (length(delta) > 1) {
    return(do.call(rbind, purrr::map(delta, power_null, n = n, theta = theta)))
  }
  p <- stats::pnorm(sqrt(n) * (-delta) - sqrt(n) * theta - 1.96) +
    stats::pnorm(-sqrt(n) * delta + sqrt(n) * theta - 1.96)
  data.frame(delta = delta, n = n, theta = theta, p = p)
}

#' Probability that \eqn{p_\delta = 1}{p\delta = 1}
#'
#' @param delta Numeric vector. The width of the indifference zone in SD units.
#' @param n Numeric. The sample size.
#' @param theta The "true" effect size.
#'
#' @return Data frame with the following columns:
#'   * delta: The width of the simulated indifference zone
#'   * n: The sample size
#'   * theta: the "true" effect size
#'   * p: The probability \eqn{p_\delta = 1}{p\delta = 1}
#'
#' @export
#' @examples
#' power_alt(
#'    delta = c(0, 1/30, 1/2, 1),
#'    n = 33,
#'    theta = seq(-3, 3, 0.01)
#'    )
#'
power_alt <- function(delta, n, theta) {
  if (length(delta) > 1) {
    return(do.call(rbind, purrr::map(delta, power_alt, n = n, theta = theta)))
  }
  p <- stats::pnorm(sqrt(n) * delta - sqrt(n) * theta - 1.96) -
    stats::pnorm(sqrt(n) * (-delta) - sqrt(n) * theta + 1.96)

  if (delta <= 1.96 * sqrt(1 / n)) {
    p <- rep(0, length(theta))
  }

  data.frame(delta = delta, n = n, theta = theta, p = p)
}

#' Probability that \eqn{0 < p_\delta < 1}{0 < p\delta < 1}
#'
#' @param delta Numeric vector. The width of the indifference zone in SD units.
#' @param n Numeric. The sample size.
#' @param theta The "true" effect size.
#'
#' @return Data frame with the following columns:
#'   * delta: The width of the simulated indifference zone
#'   * n: The sample size
#'   * theta: the "true" effect size
#'   * p: The probability \eqn{0 < p_\delta < 1}{0 < p\delta < 1}
#' @export
#'
#' @examples
#' power_inconclusive(
#'    delta = c(0, 1/30, 1/2, 1),
#'    n = 33,
#'    theta = seq(-3, 3, 0.01)
#'    )
#'
power_inconclusive <- function(delta, n, theta) {
  if (length(delta) > 1) {
    return(do.call(rbind, purrr::map(delta, power_inconclusive, n = n, theta = theta)))
  }
  p <- 1 - power_null(delta, n, theta)[["p"]] - power_alt(delta, n, theta)[["p"]]

  data.frame(delta = delta, n = n, theta = theta, p = p)
}
