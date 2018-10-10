#' Create a nicely formatted table from a linear model
#'
#' Given a general linear model (like from [stats::lm] or [stats::glm]) this
#' function makes a table intended for interactive use or publication.
#'
#' @param model A model object that is compatible with [broom::tidy].
#' @param conf_level The confidence
#' @param digits
#' @param concat_signif
#' @param odds_ratio
#' @param ...
#' @return A \code{\link[base]{data.frame}} with the following columns: the
#' name of the coefficient in the model, the value of the coefficient, the
#' confidence interval for the coefficient, and the p-value for the term.
#'
#' @export
#' @importFrom broom tidy
#' @importFrom purrr map_dbl map_chr is_null
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
toffee_tbl <- function(model, conf_level = 0.95, digits = 2,
                       concat_signif = TRUE, odds_ratio = TRUE, ...) {
  link <- model$link
  if(is_null(link)) {
    link <- family(model)$link
  }

  if(link == "logit" && odds_ratio) {
    mean_function <- exp
  } else {
    mean_function <- make.link(link)$linkinv
  }

  result <- tidy(model, conf.int = TRUE, conf.level = conf_level) %>%
    mutate(Significant = toffee_stars(p.value, ...)) %>%
    mutate_at(c("estimate", "conf.low", "conf.high"), mean_function) %>%
    mutate_at(c("estimate", "conf.low", "conf.high", "p.value"),
              round, digits = digits) %>%
    mutate(p.value = map_chr(p.value, as.character)) %>%
    mutate(p.value = map_chr(p.value, ~ if_else(.x < 0.01, "< 0.01", .x))) %>%
    mutate(CI = map2_chr(conf.low, conf.high,
                         ~ paste0("[", .x, ", ", .y, "]"))) %>%
    dplyr::select(term, estimate, CI, p.value, Significant) %>%
    rename(Variable = term, Estimate = estimate, p_value = p.value)

  if(link == "logit" && odds_ratio) {
    result <- result %>%
      rename(Odds_Ratio = Estimate)
  }

  if(concat_signif) {
    result <- result %>%
      mutate(p_value = paste0(p_value, Significant)) %>%
      select(-Significant)
  }

  result
}
