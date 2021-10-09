library(tidyverse)
library(EnvStats)
train <- readr::read_csv("data-train.csv")
test <- readr::read_csv("data-test.csv")

# Make Fr, Re factors
train <- train %>%
  mutate(across(c(Fr, Re), factor))

# Pivot to long format
train_long <- train %>%
  tidyr::unite(interaction, Fr, Re, sep = ": ") %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_prefix = "R_moment_",
               names_to = "moment_type",
               values_to = "moment_value")

# moment 1
train_long1 <- train_long %>%
  filter(moment_type == "1") %>%
  mutate(St = sqrt(St))

lmom1 <- lm(moment_value ~ poly(St, 2)*interaction, data = train_long1)

m1bc <- boxcox(lmom1)

summary(lmom1)
m1bc$lambda
m1bc$objective
# ^ Highest objective for lambda = 0

library(boot)
cv.glm(train_long %>%
         filter(moment_type == "1") %>%
         mutate(St = sqrt(St)), K = 5, lmom1)$delta[1]


# moment 2
train_long2 <- train_long %>%
  filter(moment_type == "2") %>%
  mutate(St = sqrt(St))

lmom2 <- lm(moment_value ~ poly(St, 2)*interaction, data = train_long2)


summary(lmom2)

m2bc <- boxcox(lmom2)

m2bc$lambda
m2bc$objective
# ^ Highest objective for lambda = 0 

cv.glm(train_long2, K = 5, lmom2)$delta[1]


# moment 3
train_long3 <- train_long %>%
  filter(moment_type == "3") %>%
  mutate(St = sqrt(St))

lmom3 <- lm(moment_value ~ poly(St, 2)*interaction, data = train_long3)


summary(lmom3)

m3bc <- boxcox(lmom3)

m3bc$lambda
m3bc$objective
# ^ Highest objective for lambda = 0 

# moment 4
train_long4 <- train_long %>%
  filter(moment_type == "4") %>%
  mutate(St = sqrt(St))

lmom4 <- lm(moment_value ~ poly(St, 2)*interaction, data = train_long4)


summary(lmom4)

m4bc <- boxcox(lmom4)

m4bc$lambda
m4bc$objective
# ^ Highest objective for lambda = 0 