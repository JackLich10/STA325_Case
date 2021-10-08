library(tidyverse)
library(boot)
library(MASS)
set.seed(1)

train <- readr::read_csv("data/data-train.csv")
train <- train %>%
  # change Fr to numeric
  mutate(Fr = as.numeric(Fr)) %>%
  # transforming and scaling the response variables
  mutate(across(c(R_moment_1, R_moment_2, R_moment_3, R_moment_4), scale)) %>%
  # mutate(R_moment_1 = (R_moment_1 - min(R_moment_1))/(max(R_moment_1) - min(R_moment_1)),
  #        R_moment_2 = (R_moment_2 - min(R_moment_2))/(max(R_moment_2) - min(R_moment_2)),
  #        R_moment_3 = (R_moment_3 - min(R_moment_3))/(max(R_moment_3) - min(R_moment_3)),
  #        R_moment_4 = (R_moment_4 - min(R_moment_4))/(max(R_moment_4) - min(R_moment_4))) %>%
  # making Fr and Re categorical
  mutate(across(c(Fr, Re), factor)) %>%
  # taking the square root of St
  mutate(St = sqrt(St))

# Pivot to long format
train_long <- train %>%
  tidyr::unite(interaction, Fr, Re, sep = ": ") %>%
  pivot_longer(cols = starts_with("R_moment"),
               names_prefix = "R_moment_",
               names_to = "moment_type",
               values_to = "moment_value")

# Model 1: 3 predictors
# Model 2: Interaction between Fr*Re
# Model 3: See explore.R
# consider continuous Fr & Re models?

# R_moment_1: exploring different model complexity, K = 5
x <- summary(lm(R_moment_1 ~ St + Fr + Re, data = train))
print("adj R^2")
x$adj.r.squared
model <- glm(R_moment_1 ~ St + Fr + Re, data = train)
qqnorm(model$residuals)
cv.glm(train, model, K = 5)$delta[1]

x <- summary(lm(R_moment_1 ~ St + Fr*Re, data = train))
print("adj R^2")
x$adj.r.squared
model <- glm(R_moment_1 ~ St + Fr*Re, data = train)
qqnorm(model$residuals)
cv.glm(train, model, K = 5)$delta[1]

data_1 <- train_long %>%
  filter(moment_type == "1")
x <- summary(lm(moment_value ~ poly(St, 3)*interaction, data = data_1))
print("adj R^2")
x$adj.r.squared
model <- glm(moment_value ~ poly(St, 3)*interaction, data = data_1)
qqnorm(model$residuals)
cv.glm(data_1, model, K = 5)$delta[1]



# R_moment_2: exploring different model complexity, K = 5
x <- summary(lm(R_moment_2 ~ St + Fr + Re, data = train))
print("adj R^2")
x$adj.r.squared
model <- glm(R_moment_2 ~ St + Fr + Re, data = train)
qqnorm(model$residuals)
cv.glm(train, model, K = 5)$delta[1]

x <- summary(lm(R_moment_2 ~ St + Fr*Re, data = train))
print("adj R^2")
x$adj.r.squared
model <- glm(R_moment_2 ~ St + Fr*Re, data = train)
qqnorm(model$residuals)
cv.glm(train, model, K = 5)$delta[1]

data_2 <- train_long %>%
  filter(moment_type == "2")
x <- summary(lm(moment_value ~ poly(St, 3)*interaction, data = data_2))
print("adj R^2")
x$adj.r.squared
model <- glm(moment_value ~ poly(St, 3)*interaction, data = data_2)
qqnorm(model$residuals)
cv.glm(data_2, model, K = 5)$delta[1]



# R_moment_3: exploring different model complexity, K = 5
x <- summary(lm(R_moment_3 ~ St + Fr + Re, data = train))
print("adj R^2")
x$adj.r.squared
model <- glm(R_moment_3 ~ St + Fr + Re, data = train)
qqnorm(model$residuals)
cv.glm(train, model, K = 5)$delta[1]

x <- summary(lm(R_moment_3 ~ St + Fr*Re, data = train))
print("adj R^2")
x$adj.r.squared
model <- glm(R_moment_3 ~ St + Fr*Re, data = train)
qqnorm(model$residuals)
cv.glm(train, model, K = 5)$delta[1]

data_3 <- train_long %>%
  filter(moment_type == "3")
x <- summary(lm(moment_value ~ poly(St, 3)*interaction, data = data_3))
print("adj R^2")
x$adj.r.squared
model <- glm(moment_value ~ poly(St, 3)*interaction, data = data_3)
qqnorm(model$residuals)
cv.glm(data_3, model, K = 5)$delta[1]



# R_moment_4: exploring different model complexity, K = 5
x <- summary(lm(R_moment_4 ~ St + Fr + Re, data = train))
print("adj R^2")
x$adj.r.squared
model <- glm(R_moment_4 ~ St + Fr + Re, data = train)
qqnorm(model$residuals)
cv.glm(train, model, K = 5)$delta[1]

x <- summary(lm(R_moment_4 ~ St + Fr*Re, data = train))
print("adj R^2")
x$adj.r.squared
model <- glm(R_moment_4 ~ St + Fr*Re, data = train)
qqnorm(model$residuals)
cv.glm(train, model, K = 5)$delta[1]

data_4 <- train_long %>%
  filter(moment_type == "4")
x <- summary(lm(moment_value ~ poly(St, 2)*interaction, data = data_4))
print("adj R^2")
x$adj.r.squared
model <- glm(moment_value ~ poly(St, 2)*interaction, data = data_4)
qqnorm(model$residuals)
cv.glm(data_4, model, K = 5)$delta[1]
