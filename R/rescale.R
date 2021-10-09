library(tidyverse)
library(EnvStats)
library(scales)
train <- readr::read_csv("data-train.csv")
test <- readr::read_csv("data-test.csv")

# Now using continuous St, Fr, and Re


# Rescale moments and Fr
train <- train %>% mutate(Fr = case_when(Fr == Inf ~ 100, TRUE ~ Fr),
                          R_moment_1 = rescale(train$R_moment_1, c(0, 100)),
                          R_moment_2 = rescale(train$R_moment_2, c(0, 100)),
                          R_moment_3 = rescale(train$R_moment_3, c(0, 100)),
                          R_moment_4 = rescale(train$R_moment_4, c(0, 100)),
                          St = sqrt(St)
                          )

# Pivot to long format
train_long <- train %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_prefix = "R_moment_",
               names_to = "moment_type",
               values_to = "moment_value")

# moment 1
train_long1 <- train_long %>%
  filter(moment_type == "1") 

lmom1 <- lm(moment_value ~ poly(St, 2)*Re*Fr + Re + Fr, data = train_long1)

summary(lmom1)


library(boot)
cv.glm(train_long %>%
         filter(moment_type == "1")), K = 5, lmom1)$delta[1]


# moment 2
train_long2 <- train_long %>%
  filter(moment_type == "2") 

lmom2 <- lm(moment_value ~ poly(St, 2)*Re*Fr + Re + Fr, data = train_long2)


summary(lmom2)



cv.glm(train_long2, K = 5, lmom2)$delta[1]


# moment 3
train_long3 <- train_long %>%
  filter(moment_type == "3") 

lmom3 <- lm(moment_value ~ poly(St, 2)*Re*Fr + Re + Fr, data = train_long3)


summary(lmom3)


# moment 4
train_long4 <- train_long %>%
  filter(moment_type == "4")

lmom4 <- lm(moment_value ~ poly(St, 2)*Re*Fr + Re + Fr, data = train_long4)


summary(lmom4)

