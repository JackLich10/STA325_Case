### cross-fold validation

library(tidyverse)
train <- readr::read_csv("data/data-train.csv", col_types = readr::cols()) %>%
  dplyr::mutate(Fr = as.numeric(Fr))
test <- readr::read_csv("data/data-test.csv", col_types = readr::cols()) %>%
  dplyr::mutate(Fr = as.numeric(Fr))

# Make Fr, Re a factor level
train <- train %>%
  tidyr::unite(interaction, Fr, Re, sep = ": ", remove = FALSE)

#### split into train and test
set.seed(123)
spl <- rsample::initial_split(train, prop = 0.8)
tr <- rsample::training(spl)
te <- rsample::testing(spl)

# create folds
set.seed(234)
folds <- rsample::vfold_cv(tr, v = 5)

# function to train on a given fold, using given degree, predicting given moment
lm_cv <- function(fold, degree, moment, sqrt = TRUE, log = TRUE) {
  data <- rsample::analysis(folds$splits[[fold]]) %>%
    dplyr::mutate(target = !!dplyr::sym(paste0("R_moment_", moment)))
  assess <- rsample::assessment(folds$splits[[fold]]) %>%
    dplyr::mutate(target = !!dplyr::sym(paste0("R_moment_", moment)))

  if (isTRUE(sqrt)) {
    data <- data %>%
      dplyr::mutate(St = sqrt(St))
    assess <- assess %>%
      dplyr::mutate(St = sqrt(St))
  }

  if (isTRUE(log)) {
    data <- data %>%
      dplyr::mutate(target = log(target))
    assess <- assess
  }

  mod <- data %>%
    lm(target ~ poly(St, degree)*interaction, data = .)

  train_adj_r2 <- broom::glance(mod)$adj.r.squared

  assess <- assess %>%
    dplyr::mutate(pred = predict(mod, ., type = "response") %>% as.numeric())

  if (isTRUE(log)) {
    assess <- assess %>%
      dplyr::mutate(pred = exp(pred))
  }
  return(assess)
}

assess %>%
  ggplot(aes(pred, target)) +
  geom_point() +
  geom_abline()

# compute CV
cv <- tidyr::crossing(fold = 1:5,
                      degree = 1:3,
                      moment = 1:4,
                      sqrt = c(TRUE, FALSE),
                      log = c(TRUE, FALSE)) %>%
  dplyr::mutate(cv = purrr::pmap(list(fold, degree, moment, sqrt, log),
                                 ~ lm_cv(fold = ..1,
                                         degree = ..2,
                                         moment = ..3,
                                         sqrt = ..4,
                                         log = ..5))) %>%
  tidyr::unnest(cv)

# Plot
cv %>%
  filter(sqrt == TRUE) %>%
  # filter(moment == 3) %>%
  # filter(degree < 3) %>%
  dplyr::group_by(moment = factor(moment), degree, sqrt, log) %>%
  dplyr::summarise(rmse = sqrt(mean((pred-target)^2)),
                   mae = mean(abs(pred-target)),
                   broom::glance(lm(target ~ pred, data = dplyr::cur_data())),
                   .groups = "drop") %>%
# %>% view(title = "new")
  dplyr::mutate(moment = paste0("Moment: ", moment)) %>%
  tidyr::pivot_longer(cols = c(rmse, mae, adj.r.squared)) %>%
  tidyr::unite(type, moment, name, sep = ": ", remove = FALSE) %>%
  ggplot(aes(degree, value, color = moment, lty = log)) +
  geom_line() +
  geom_point() +
  guides(color = "none") +
  facet_wrap(~ type, scales = "free", nrow = 4)

#### degree of polynomial 2 probably makes the most sense!
### Note that we still need to sqrt() and then do poly(, degree = 2)
### since poly() is different than just ^2

train_long %>%
  ggplot(aes(log(moment_value))) +
  geom_histogram() +
  facet_wrap(~ moment_type, scales = "free_x")
