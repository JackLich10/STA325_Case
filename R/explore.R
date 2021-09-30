library(tidyverse)
train <- readr::read_csv("casestudy/data-train.csv")
test <- readr::read_csv("casestudy/data-test.csv")

# histograms of predictors
train %>%
  pivot_longer(cols = c(St:Fr),
               names_to = "metric",
               values_to = "value") %>%
  filter(value != Inf) %>%
  ggplot(aes(value, R_moment_1)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free")

# These should really be factors, not numeric
train %>% count(Fr)

train %>% count(Re)

# Make Fr, Re factors
train <- train %>%
  mutate(across(c(Fr, Re), factor))

# Plot St vs. R_moment_1, color by interaction between Fr, Re
train %>%
  filter(!(Fr == 0.052 & Re == 90)) %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_to = "moment",
               values_to = "value") %>%
  ggplot(aes(St, value, color = interaction(Fr, Re))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ moment, scales = "free_y")

# These are very strong relationships

# Should likely take square root of `St`, since distribution becomes more normal when doing so
train %>%
  mutate(St = sqrt(St)) %>%
  ggplot(aes(St)) +
  geom_histogram(bins = 20)

# Pivot to long format
train_long <- train %>%
  tidyr::unite(interaction, Fr, Re, sep = ": ") %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_prefix = "R_moment_",
               names_to = "moment_type",
               values_to = "moment_value")

# moment 1
train_long %>%
  filter(moment_type == "1") %>%
  mutate(St = sqrt(St)) %>%
  lm(moment_value ~ poly(St, 3)*interaction, data = .) %>%
  summary()
  ggplot(aes(St, moment_value, color = moment_type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ interaction, scales = "free")

# moment 2
train_long %>%
    filter(moment_type == "2") %>%
    mutate(St = sqrt(St)) %>%
    lm(moment_value ~ poly(St, 3)*interaction, data = .) %>%
    summary()
  ggplot(aes(St, moment_value, color = moment_type)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ interaction, scales = "free")

  # moment 3
  train_long %>%
    filter(moment_type == "3") %>%
    mutate(St = sqrt(St)) %>%
    lm(moment_value ~ poly(St, 3)*interaction, data = .) %>%
    summary()
  ggplot(aes(St, moment_value, color = moment_type)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ interaction, scales = "free")

# moment 4
  train_long %>%
    filter(moment_type == "4") %>%
    mutate(St = sqrt(St)) %>%
    lm(moment_value ~ poly(St, 2)*interaction, data = .) %>%
    summary()
  ggplot(aes(St, moment_value, color = moment_type)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ interaction, scales = "free")


train %>%
  mutate(St = sqrt(St)) %>%
  ggplot(aes(St)) +
  geom_histogram(bins = 20)
