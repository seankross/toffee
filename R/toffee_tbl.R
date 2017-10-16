#' Create a pretty table from a model
#'
#' Given a general linear model (from \code{lm} or \code{glm}) this function
#' makes a pretty table intended for communication.
#'
#' @param model A model object that is compatible with \code{\link[broom]{tidy}}.
#' @return A \code{\link[base]{data.frame}} with the following columns: the
#' name of the coefficient in the model, the value of the coefficient, the
#' confidence interval for the coefficient, and the p-value for the term.
#'
#' @export
#' @importFrom broom tidy
#' @importFrom purrr map_dbl map_chr
#' @importFrom dplyr mutate select "%>%"
#' @examples
#'
#' library(dplyr)
#'
#' set.seed(2017-10-15)
#'
#' trees %>%
#'   mutate(Branches = sample(15:25, size = 31, replace = TRUE)) %>%
#'   lm(Volume ~ Girth + Height + Branches, data = .) %>%
#'   toffee_tbl()
#'
#' # term               Coef               CI p.value
#' # (Intercept) -67.1978389 [-85.93, -48.46] < 0.01*
#' # Girth         4.7531349     [4.26, 5.25] < 0.01*
#' # Height        0.3311483     [0.09, 0.58]   0.01*
#' # Branches      0.4600241    [-0.01, 0.93]    0.07
toffee_tbl <- function(model) {
  lf <- compliment_link(family(model)$link)

  tidy(model) %>%
    mutate(Star = ifelse(p.value < 0.05, "*", "")) %>%
    mutate(Coef = map_dbl(estimate, lf)) %>%
    mutate(p.value = map_dbl(p.value, round, digits = 2)) %>%
    mutate(p.value = map_chr(p.value, as.character)) %>%
    mutate(p.value = map_chr(p.value, ~ ifelse(.x < 0.01, "< 0.01", .x))) %>%
    mutate(p.value = paste0(p.value, Star)) %>%
    mutate(CI = toffee_ci(Coef, std.error, lf)) %>%
    dplyr::select(term, Coef, CI, p.value)
}
