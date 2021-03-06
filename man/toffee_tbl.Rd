% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toffee_tbl.R
\name{toffee_tbl}
\alias{toffee_tbl}
\title{Create a nicely formatted table from a model object.}
\usage{
toffee_tbl(model, conf_level = 0.95, digits = 2,
  concat_signif = TRUE, odds_ratio = TRUE, ci_fmt = TRUE, ...)
}
\arguments{
\item{model}{A model object that is compatible with \code{\link[broom:tidy]{broom::tidy()}}.}

\item{conf_level}{The confidence level that will be used for computing the
confidence interval (for example: \code{0.95} or \code{0.99}).}

\item{digits}{The number of decimal places that numbers in the table should
be rounded to. For no rounding use \code{Inf}. This argument is provided to
\code{\link[base:round]{base::round()}}. The default value is \code{2}.}

\item{concat_signif}{If \code{TRUE} the resulting data frame will append
significance symbols as generated by \code{\link[=toffee_signif]{toffee_signif()}}. If \code{FALSE} the
significance symbols will appear in a separate column called \code{Significant}.}

\item{odds_ratio}{If \code{TRUE} and the family of the model provided is
\code{logit} then the model coefficients and confidence intervals returned in
the resulting table will be odds ratios.}

\item{ci_fmt}{If \code{TRUE} then the confidence interval will be formatted between
brackets in one column called \code{CI}. If \code{FALSE} the lower and upper bounds of
the confidence interval will be in their own columns called \code{Lower_CI} and
\code{Upper_CI} respectfully.}

\item{...}{Arguments that will be passed to \code{\link[=toffee_signif]{toffee_signif()}}.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} with the following columns: the
name of the coefficient in the model (\code{Variable}), the value of the
coefficient with inverse of the link function applied (\code{Estimate} or
\code{Odds_Ratio}), the confidence interval for the coefficient (\code{CI}), the
p-value for the estimate, (\code{p_value}), and optionally symbols representing
levels of significance (\code{Significant}).
}
\description{
Given a model object (like from \code{\link[stats:lm]{stats::lm()}}, \code{\link[stats:glm]{stats::glm()}}, or
\code{\link[lme4:glmer]{lme4::glmer()}}) this function makes a table intended for interactive use or
publication. The model coefficients and confidence intervals displayed in the
table have had their \href{https://en.wikipedia.org/wiki/Generalized_linear_model#Link_function}{inverse link function}
applied, therefore they are in the units of the response variable.
}
\examples{

library(dplyr)
library(toffee)

linear_model <- toffee_forest \%>\%
  lm(Volume ~ Girth + Height + Branches, data = .)

# Basic usage
linear_model \%>\%
  toffee_tbl()

 # # A tibble: 4 x 4
 #   Variable    Estimate CI               p_value
 #   <chr>          <dbl> <chr>            <chr>
 # 1 (Intercept)   -67.2  [-86.81, -47.59] < 0.01***
 # 2 Girth           4.75 [4.23, 5.27]     < 0.01***
 # 3 Height          0.33 [0.08, 0.59]     0.01*
 # 4 Branches        0.46 [-0.03, 0.95]    0.07

# Using the `chars` argument from toffee_signif
linear_model \%>\%
  toffee_tbl(thresholds = 0.01, chars = "*")

 # # A tibble: 4 x 4
 #   Variable    Estimate CI               p_value
 #   <chr>          <dbl> <chr>            <chr>
 # 1 (Intercept)   -67.2  [-86.81, -47.59] < 0.01*
 # 2 Girth           4.75 [4.23, 5.27]     < 0.01*
 # 3 Height          0.33 [0.08, 0.59]     0.01
 # 4 Branches        0.46 [-0.03, 0.95]    0.07

# Separating the significance symbols into their own column
linear_model \%>\%
  toffee_tbl(concat_signif = FALSE)

 # # A tibble: 4 x 5
 #   Variable    Estimate CI               p_value Significant
 #   <chr>          <dbl> <chr>            <chr>   <chr>
 # 1 (Intercept)   -67.2  [-86.81, -47.59] < 0.01  ***
 # 2 Girth           4.75 [4.23, 5.27]     < 0.01  ***
 # 3 Height          0.33 [0.08, 0.59]     0.01    *
 # 4 Branches        0.46 [-0.03, 0.95]    0.07    ""

logistic_model <- toffee_forest \%>\%
  glm(Healthy ~ Girth + Height + Branches + Volume, data = .,
      family = binomial())

logistic_model \%>\%
  toffee_tbl()

 # # A tibble: 5 x 4
 #   Variable    Odds_Ratio CI                  p_value
 #   <chr>            <dbl> <chr>               <chr>
 # 1 (Intercept)       0.05 [0, 10626998633.06] 0.81
 # 2 Girth             0.75 [0.14, 3.65]        0.72
 # 3 Height            0.92 [0.73, 1.14]        0.45
 # 4 Branches          2    [1.31, 3.82]        0.01**
 # 5 Volume            1.03 [0.75, 1.45]        0.86

}
