#' Specify symbols to represent confidence levels.
#'
#' @param p_values A numeric vector of p-values.
#' @param thresholds Descending p-value thresholds. Values less than or equal to
#'  a threshold will produce the corresponding character.
#' @param chars A vector of strings that symbolize a p-value less than the
#' threshold.
#' @export
#' @examples
#'
#' toffee_signif(c(1, .5, .049, .01, .0012, 0))
toffee_signif <- function(p_values,
                         thresholds = c(0.05, 0.01, 0.001),
                         chars = c("*", "**", "***")) {

  if(length(thresholds) != length(chars)) {
    stop("[toffee_signif] Please ensure that there are the same number of thresholds and character symbols.",
         call. = FALSE)
  }

  symnum(p_values, cutpoints = c(0, rev(thresholds), 1),
         symbols = c(rev(chars), ""),
         corr = FALSE, na = FALSE) %>%
    as.character()
}
