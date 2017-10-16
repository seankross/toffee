#' Make pretty two-sided 95pct confidence intervals
#'
#' @param m A vector of sample means.
#' @param se A vector of sample standard errors.
#' @param cf A compliment to a link function. For example use
#' \code{\link[base]{I}} for linear models or \code{\link[base]{exp}} for
#' a binomial model.
#' @return A vector of strings with formatted confidence intervals.
#' @importFrom dplyr data_frame mutate pull "%>%"
#' @importFrom purrr map_dbl map_chr compose map2_chr
#' @export
#' @examples
#' toffee_ci(c(1, 2, 3), c(.2, .25, .3), I)
#' # "[0.61, 1.39]" "[1.51, 2.49]" "[2.41, 3.59]"
#'
#' toffee_ci(c(4, 2, 7), c(.15, .2, .1), exp)
#' # "[3.71, 4.29]" "[1.61, 2.39]" "[6.8, 7.2]"
toffee_ci <- function(m, se, cf) {
  data_frame(m = m, se = se) %>%
    mutate(lower = m - qnorm(.975) * se) %>%
    mutate(upper = m + qnorm(.975) * se) %>%
    mutate(lower = map_dbl(lower, cf)) %>%
    mutate(upper = map_dbl(upper, cf)) %>%
    mutate(lower = map_dbl(lower, round, digits = 2)) %>%
    mutate(upper = map_dbl(upper, round, digits = 2)) %>%
    mutate(ci = map2_chr(lower, upper, ~ paste0("[", .x, ", ", .y, "]"))) %>%
    pull(ci)
}
