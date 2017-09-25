#' Second Generation P-value
#'
#' @param lb Numeric. Lower bound of the observed effect
#' @param ub Numeric. Upper bound of the observed effect
#' @param delta_a Numeric. Lower bound of the indifference zone
#' @param delta_b Numeric. Upper bound of the indifference zone
#'
#' @return Numeric. Second generation p-value for the observed effect.
#' @export
#'
#' @examples
#' ## Second Generation P-value for a confidence interval of (140, 145) and an
#' ## indifference zone of (144, 148)
#' second_gen_pvalue(140, 145, 144, 148)
p_delta <- function(lb, ub, delta_a, delta_b) {
  interval <- ub - lb
  null <- delta_b - delta_a

  intersect <- min(delta_b, max(ub, delta_a)) - max(delta_a, min(lb, delta_b))

  correct <- max(0.5 * interval / null, 1)

  intersect / interval * correct
}
