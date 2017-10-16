# Toffee

## Installation

```r
devtools::install_github("seankross/toffee")
```

## Usage

```r
##################
## toffee_tbl() ##
##################

library(dplyr)

set.seed(2017-10-15)

trees %>%
  mutate(Branches = sample(15:25, size = 31, replace = TRUE)) %>%
  lm(Volume ~ Girth + Height + Branches, data = .) %>%
  toffee_tbl()

# term               Coef               CI p.value
# (Intercept) -67.1978389 [-85.93, -48.46] < 0.01*
# Girth         4.7531349     [4.26, 5.25] < 0.01*
# Height        0.3311483     [0.09, 0.58]   0.01*
# Branches      0.4600241    [-0.01, 0.93]    0.07

#################
## toffee_ci() ##
#################

toffee_ci(c(1, 2, 3), c(.2, .25, .3), I)
# "[0.61, 1.39]" "[1.51, 2.49]" "[2.41, 3.59]"

toffee_ci(c(4, 2, 7), c(.15, .2, .1), exp)
# "[3.71, 4.29]" "[1.61, 2.39]" "[6.8, 7.2]"
```
