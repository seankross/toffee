library(dplyr)
library(devtools)
library(toffee)

set.seed(2017-10-15)

toffee_forest <- trees %>%
  mutate(Branches = sample(15:25, size = 31, replace = TRUE)) %>%
  mutate(Healthy = Branches > 20) %>%
  mutate(Healthy = Healthy + sample(c(0, 1), size = nrow(.), replace = TRUE)) %>%
  mutate(Healthy = Healthy > 0)

use_data(toffee_forest)
