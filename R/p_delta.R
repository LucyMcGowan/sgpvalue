#' Second Generation P-value
#'
#' @param lb Numeric. Lower bound of the observed effect
#' @param ub Numeric. Upper bound of the observed effect
#' @param delta_lb Numeric. Lower bound of the indifference zone
#' @param delta_ub Numeric. Upper bound of the indifference zone
#'
#' @return Numeric. Second generation p-value for the observed effect.
#' @export
#'
#' @examples
#' ## Second Generation P-value for a confidence interval of (140, 145) and an
#' ## indifference zone of (144, 148)
#' p_delta(140, 145, 144, 148)
p_delta <- function(lb, ub, delta_lb, delta_ub) {

  # special case: infinite CI and H0 bounds in the same direction
  if ((delta_lb == -Inf & lb == -Inf) | (delta_ub == Inf & ub == Inf)) {
    return(1)
  }

  # usual case: non-point CI & non-point Ho
  # pdelta = |CI intersect Ho| / min{ |CI|, 2|Ho| }
  if (delta_lb != delta_ub & lb != ub) {
    if (lb > delta_ub | ub < delta_lb) {
      return(0)
    } else if(lb > delta_lb & ub < delta_ub){
      return(1)
    } else {
      return(
        (min(ub, delta_ub) - max(lb, delta_lb)) /
          min(ub - lb, 2 * (delta_ub - delta_lb))
      )
    }
  }

  # special case 1: point CI, w/ or w/out a point H0
  # pdelta = 0 if CI is inside the Ho
  # pdelta = 1 if CI is inside the Ho
  if (lb == ub) {
    if (lb <= delta_ub & lb >= delta_lb){
      return(1)
    } else {
      return(0)
    }
  }

  # special case 2: point H0 & non-point CI
  # pdelta = 1/2 if H0 is inside the CI
  # pdelta = 0 if H0 is outside the CI
  if (delta_lb == delta_ub & lb != ub) {
    if (delta_lb <= ub & delta_lb >= lb) {
      return(1/2)
    } else {
      return(0)
    }
  }
}
