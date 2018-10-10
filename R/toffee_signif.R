
#' @param p_values A numeric vector of p-values.
#' @param thesholds Descending p-value thresholds. Values less than or equal to
#'  a threshold will produce the corresponding character.
#' @param chars A vector of strings that symbolize a p-value less than the
#' threshold.
toffee_signif <- function(p_values,
                         thresholds = c(0.05, 0.01, 0.001),
                         chars = c("*", "**", "***")) {
  symnum(p_values, cutpoints = c(0, rev(thresholds), 1),
         symbols = c(rev(chars), ""),
         corr = FALSE, na = FALSE) %>%
    as.character()
}

toffee_stars(c(1, .5, .049, .01, .0012, 0))
