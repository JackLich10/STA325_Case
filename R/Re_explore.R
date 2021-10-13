library(tidyverse)
train <- readr::read_csv("~/Desktop/REnvironment/325/STA325_Case/data/data-train.csv")
test <- readr::read_csv("~/Desktop/REnvironment/325/STA325_Case/data/data-test.csv")

# histograms of predictors
train %>%
  pivot_longer(cols = c(St:Fr),
               names_to = "metric",
               values_to = "value") %>%
  filter(value != Inf) %>%
  ggplot(aes(value, R_moment_1)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free")

# These each have 3 levels but we may want to leave Re as numerical for wider prediction
train %>% count(Fr)
train %>% count(Re)

# Make Fr, Re factors
train <- train %>%
  mutate(across(c(Fr), factor))

# Interactions
train %>% 
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = interaction(Re, Fr))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
train %>%
  filter(!(near(Re, 90) & Fr=="0.052")) %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = interaction(Re, Fr))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
train %>%
  filter(!(near(Re, 90) & Fr=="0.052")) %>%
  filter(!(near(Re, 224) & Fr=="0.052")) %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = interaction(Re, Fr))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")

# Plot St vs. R_moment_1, color by interaction between Fr, Re
train %>% #why were some filtered out
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = Re)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
train %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = Fr)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
train %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(Re, value, color = St)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
train %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(Re, value, color = Fr)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")

# Re*St stronger than Fr*St
# Inf is v diffrent from other Fr values

hist(train$St)
hist(train$Re)
train %>% count(Fr)

# Re and Fr are roughly evenly distributed
# St is very right skewed with most values less than 1

#Rel w moments
train %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(Re, value)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
# nonlinear, nonconstant variance either, for higher Re, much smaller variance and lower values

train %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
# St and Re need an interaction, likely log or something like that since it smooths out
# one curve up that slows, others very low, increasing variance

train %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(Fr, value)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
# for moment 1, decent distr for all, for others, v low for 0.3 and inf

# Pivot to long format
train_long <- train %>%
  tidyr::unite(interaction, Fr, Re, sep = ": ") %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_prefix = "R_moment_",
               names_to = "moment_type",
               values_to = "moment_value")

# try fr^2
# or fr accel cubic? 
# standardize over size? st or Re is the largest
# try categorizing fr into <1, =1, >1
# nonlinear
# what's more complex than x1*x2, x1^2*x2???
# lognormal distributions? (0, inf)

# -------------------------------------------------------------------------------------
train <- readr::read_csv("~/Desktop/REnvironment/325/STA325_Case/data/data-train.csv")

# change moments
train <- train %>%
  mutate(
    central_1 = R_moment_1, #mean
    central_2 = R_moment_2 - R_moment_1^2, #var
    central_3 = (R_moment_3 - 3*R_moment_2*R_moment_1 + 2*R_moment_1^3), #skew
    central_4 = (R_moment_4 - 4*R_moment_3*R_moment_1 + 6*R_moment_2*R_moment_1^2 - 3*R_moment_1^4)) #kurtosis

# Interactions
train %>% 
  pivot_longer(cols = starts_with("central"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = interaction(Re, Fr))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
train %>%
  filter(!(near(Re, 90) & Fr=="0.052")) %>%
  pivot_longer(cols = starts_with("central"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = interaction(Re, Fr))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")
train %>%
  filter(!(near(Re, 90) & Fr=="0.052")) %>%
  filter(!(near(Re, 224) & Fr=="0.052")) %>%
  pivot_longer(cols = starts_with("central"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = interaction(Re, Fr))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")

# -------------------------------------------------------------------------------------
# Pred

set.seed(123)
train$index <- 1:nrow(train)
spl <- rsample::initial_split(train, prop = 0.8)
tr <- rsample::training(spl)
te <- rsample::testing(spl)

# create folds
set.seed(234)
folds <- rsample::vfold_cv(tr, v = 5)

lm_cv <- function(fold, degree, moment, sqrt = TRUE, log = TRUE, central = TRUE) {
  data <- rsample::analysis(folds$splits[[fold]]) %>%
    dplyr::mutate(target = !!dplyr::sym(paste0("R_moment_", moment)),
                  central_target = !!dplyr::sym(paste0("central_", moment))) %>% 
    tidyr::unite(interaction, Fr, Re, sep = ": ", remove = FALSE)
  assess <- rsample::assessment(folds$splits[[fold]]) %>%
    dplyr::mutate(target = !!dplyr::sym(paste0("R_moment_", moment)),
                  central_target = !!dplyr::sym(paste0("central_", moment))) %>% 
    tidyr::unite(interaction, Fr, Re, sep = ": ", remove = FALSE)
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
  if (central) {
    mod <- data %>%
      lm(central_target ~ poly(St, degree)*interaction, data = .)
  }
  
  #train_adj_r2 <- broom::glance(mod)$adj.r.squared
  
  assess <- assess %>%
    dplyr::mutate(pred = predict(mod, ., type = "response") %>% as.numeric())
  
  if (isTRUE(log)) {
    assess <- assess %>%
      dplyr::mutate(pred = exp(pred))
  }
  return(assess)
}

cv <- tidyr::crossing(fold = 1:5,
                      degree = 2,
                      moment = 1:4,
                      sqrt = c(TRUE),
                      log = c(FALSE),
                      central = c(TRUE, FALSE)) %>%
  dplyr::mutate(cv = purrr::pmap(list(fold, degree, moment, sqrt, log, central),
                                 ~ lm_cv(fold = ..1,
                                         degree = ..2,
                                         moment = ..3,
                                         sqrt = ..4,
                                         log = ..5,
                                         central = ..6))) %>%
  tidyr::unnest(cv)

undo_central <- function(cv) {
  cv_wide <- cv %>%
    dplyr::group_by(index, fold, degree, sqrt, log, central) %>%
    summarise(moment, pred)
  cv_wide <- cv_wide %>%
    reshape2::dcast(index + fold + degree + sqrt + log + central ~ moment, value.var="pred")
  
  for (i in 1:nrow(cv_wide)) {
    if (cv_wide[i, ]$central) {
      cv_wide[i, "2"] = cv_wide[i, "2"] + cv_wide[i, "1"]^2
      cv[(cv["index"] == cv_wide[i, "index"]) & (cv["fold"] == cv_wide[i, "fold"]) & (cv["degree"] == cv_wide[i, "degree"]) & (cv["sqrt"] == cv_wide[i, "sqrt"]) & (cv["log"] == cv_wide[i, "log"]) & (cv["central"] == cv_wide[i, "central"]) & (cv["moment"] == 2), "pred"] = cv_wide[i, "2"] 
      cv_wide[i, "3"] = cv_wide[i, "3"] - 2*cv_wide[i, "1"]^3 + 3*cv_wide[i, "2"]*cv_wide[i, "1"]
      cv[(cv["index"] == cv_wide[i, "index"]) & (cv["fold"] == cv_wide[i, "fold"]) & (cv["degree"] == cv_wide[i, "degree"]) & (cv["sqrt"] == cv_wide[i, "sqrt"]) & (cv["log"] == cv_wide[i, "log"]) & (cv["central"] == cv_wide[i, "central"]) & (cv["moment"] == 3), "pred"] = cv_wide[i, "3"]
      cv_wide[i, "4"] = cv_wide[i, "4"] + 3*cv_wide[i, "1"]^4 - 6*cv_wide[i, "2"]*cv_wide[i, "1"]^2 + 4*cv_wide[i, "3"]*cv_wide[i, "1"]
      cv[(cv["index"] == cv_wide[i, "index"]) & (cv["fold"] == cv_wide[i, "fold"]) & (cv["degree"] == cv_wide[i, "degree"]) & (cv["sqrt"] == cv_wide[i, "sqrt"]) & (cv["log"] == cv_wide[i, "log"]) & (cv["central"] == cv_wide[i, "central"]) & (cv["moment"] == 4), "pred"] = cv_wide[i, "4"]
    }
  }
  
  return(cv)
}

cv_summarized <- cv %>%
  dplyr::group_by(moment = factor(moment), degree, sqrt, log, central) %>%
  dplyr::summarise(rmse = sqrt(mean((pred-target)^2)),
                   mae = mean(abs(pred-target)),
                   broom::glance(lm(target ~ pred, data = dplyr::cur_data())),
                   .groups = "drop")

cv_summarized %>% 
  filter(log == FALSE) %>% 
  dplyr::mutate(moment = paste0("Moment: ", moment)) %>%
  tidyr::pivot_longer(cols = c(rmse, mae, adj.r.squared)) %>%
  tidyr::unite(type, moment, name, sep = ": ", remove = FALSE) %>%
  ggplot(aes(degree, value, color = central)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ type, scales = "free", nrow = 4)

# ---------------------------------------------------------------------------------------------------
# For extrapolation
library(splines)
train <- train %>%
  mutate(Fr = 2/pi*atan(Fr), FrRe = Fr*Re)

set.seed(123)
train$index <- 1:nrow(train)
spl <- rsample::initial_split(train, prop = 0.8)
tr <- rsample::training(spl)
te <- rsample::testing(spl)

# create folds
set.seed(234)
folds <- rsample::vfold_cv(tr, v = 5)
           
lm_cv <- function(fold, degree, moment, root = 2, log = TRUE, extrapolate = TRUE) {
  data <- rsample::analysis(folds$splits[[fold]]) %>%
    dplyr::mutate(target = !!dplyr::sym(paste0("R_moment_", moment))) %>% 
    tidyr::unite(interaction, Fr, Re, sep = ": ", remove = FALSE)
  assess <- rsample::assessment(folds$splits[[fold]]) %>%
    dplyr::mutate(target = !!dplyr::sym(paste0("R_moment_", moment))) %>% 
    tidyr::unite(interaction, Fr, Re, sep = ": ", remove = FALSE)
  
  data <- data %>%
      dplyr::mutate(St = St^(1/root))
    assess <- assess %>%
      dplyr::mutate(St = St^(1/root))
  
  if (isTRUE(log)) {
    data <- data %>%
      dplyr::mutate(target = log(target))
    assess <- assess
  }
  
  if (extrapolate) {
    mod <- data %>%
      lm(target ~ ns(St, df=degree)*Re*Fr, data= .)
    print(summary(mod))
  } else {
    mod <- data %>%
      lm(target ~ poly(St, degree)*interaction(), data = .)
  }
  
  assess <- assess %>%
    dplyr::mutate(pred = predict(mod, ., type = "response") %>% as.numeric())
  
  if (isTRUE(log)) {
    assess <- assess %>%
      dplyr::mutate(pred = exp(pred))
  }
  return(assess)
}

cv <- tidyr::crossing(fold = 1:5,
                      degree = c(1, 2),
                      moment = 1:4,
                      root = c(1, 2, 3, 4, 5, 6),
                      log = c(FALSE),
                      extrapolate = c(TRUE)) %>%
  dplyr::mutate(cv = purrr::pmap(list(fold, degree, moment, root, log, extrapolate),
                                 ~ lm_cv(fold = ..1,
                                         degree = ..2,
                                         moment = ..3,
                                         root = ..4,
                                         log = ..5,
                                         extrapolate = ..6))) %>%
  tidyr::unnest(cv)

cv_summarized <- cv %>%
  dplyr::group_by(moment = factor(moment), degree, root, log, extrapolate) %>%
  dplyr::summarise(rmse = sqrt(mean((pred-target)^2)),
                   mae = mean(abs(pred-target)),
                   broom::glance(lm(target ~ pred, data = dplyr::cur_data())),
                   .groups = "drop")

cv_summarized %>% 
  filter(log == FALSE) %>% 
  dplyr::mutate(moment = paste0("Moment: ", moment)) %>%
  tidyr::pivot_longer(cols = c(rmse, mae, adj.r.squared)) %>%
  tidyr::unite(type, moment, name, sep = ": ", remove = FALSE) %>%
  ggplot(aes(root, value, color = as.factor(degree))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ type, scales = "free", nrow = 4)
           
           