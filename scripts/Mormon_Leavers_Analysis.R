# Title: Regression Analysis Predicting Church Leavers
# Author: Alex Bass
# Last Modified: 11 Feb 2024

##### Step 1: Setup #####

# Loading packages
library(tidyverse)
library(haven)

# Read in data
data <- read_sav('./data/pew-rls-2015/data')
# Download data here: https://www.pewresearch.org/dataset/pew-research-center-2014-u-s-religious-landscape-study/

# Create my disaffiliated and nondisaffiliated groups
disaffiliated <- data |> 
  filter(qj1 == 3 & RELTRAD != 20000) |> 
  mutate(
    leave = 1
  )

stayed <- data |> 
  filter(qj1 == 3 & RELTRAD == 20000) |> 
  mutate(
    leave = 0
  )

# combine both groups into one dataset
all_data <- data.table::rbindlist(list(
  stayed,
  disaffiliated
)) |> 
  as_tibble()

# collapsing sparse categories and setting missing values to NA
all_data$marital_rc <- as.numeric(car::recode(all_data$marital, "4:5=2;6=4;9=NA"))
all_data$marital_rc2 <- as.numeric(car::recode(all_data$marital, "4:5=3;6=4;9=NA"))
all_data$age_rc <- as.numeric(car::recode(all_data$agerec, "99=NA"))
all_data$age_rc_other <- as.numeric(car::recode(all_data$agerec, "1:2=1;3:5=2;6:9=3;10:15=4;99=NA"))
all_data$educ_rc <- as.numeric(car::recode(all_data$educ, "1:3=1;4:5=2;6=3;7:8=4;9=NA"))
all_data$non_white <- as.numeric(car::recode(all_data$racethn, "1=0;2:4=1;9=NA"))
all_data$utah <- ifelse(all_data$state == 49,1,0)
all_data$SEX <- as.numeric(car::recode(all_data$SEX, "1=0;2=1"))

vars_in_model <- c(
  'marital_rc',
  'marital_rc2',
  'age_rc',
  'educ_rc',
  'SEX',
  'non_white',
  'leave'
)

#dropping missing data data
all_data <- all_data |> 
  select(all_of(vars_in_model)) |> 
  drop_na()

##### Step 2: Estimate Models #####

#Base All Mormon Model
base_all_mormon_model <- glm(
  "leave ~ factor(marital_rc) + age_rc + educ_rc + SEX + non_white",
  data = all_data,
  family = binomial()
)

# Demographic Model Using Education + Sex interaction
all_mormon_model <- glm(
  "leave ~ factor(marital_rc) + age_rc + educ_rc*SEX + non_white",
  data = all_data,
  family = binomial()
)
summary(all_mormon_model)

# Demographic Model Using Additional Interaction of Marital Status + Age
all_mormon_model2 <- glm(
  "leave ~ factor(marital_rc)*age_rc + educ_rc*SEX + non_white",
  data = all_data,
  family = binomial()
)

summary(all_mormon_model2)

# Demographic Model Using alternate recoding of marital status
all_mormon_model3 <- glm(
  "leave ~ factor(marital_rc2)*age_rc + educ_rc*SEX + non_white",
  data = all_data,
  family = binomial()
)

summary(all_mormon_model3)

##### Step 3: Model Diagnostics #####

### Base All Mormon Model:

predictors <- c(
  'marital_rc',
  'age_rc',
  'educ_rc',
  'SEX',
  'non_white'
)

#Summary
summary(base_all_mormon_model)

# plot residuals
for (var in predictors){
  plot(all_data[[var]], residuals(base_all_mormon_model, type = "deviance"),
       xlab = paste("Predictor", var), ylab = "Deviance Residuals",
       main = "Deviance Residuals vs. Predictor x")
  abline(h = 0, lty = 2)
}

# Cooks D + outlier plot
car::influencePlot(base_all_mormon_model)

# Measure of fit
(rmse <- sqrt(mean((base_all_mormon_model$fitted.values - all_data$leave)^2)))

### All Mormon Model: With Education+sex interaction

#Summary
summary(all_mormon_model)

# plot residuals
for (var in predictors){
  plot(all_data[[var]], residuals(all_mormon_model, type = "deviance"),
       xlab = paste("Predictor", var), ylab = "Deviance Residuals",
       main = "Deviance Residuals vs. Predictor x")
  abline(h = 0, lty = 2)
}

# Cooks D + outlier plot
car::influencePlot(all_mormon_model)

# Measure of fit
(rmse <- sqrt(mean((all_mormon_model$fitted.values - all_data$leave)^2)))

### All Mormon Model 2: With Education+sex interaction AND Age+marital interaction
#Summary
summary(all_mormon_model2)

# plot residuals
for (var in predictors){
  plot(all_data[[var]], residuals(all_mormon_model, type = "deviance"),
       xlab = paste("Predictor", var), ylab = "Deviance Residuals",
       main = "Deviance Residuals vs. Predictor x")
  abline(h = 0, lty = 2)
}

# Cooks D + outlier plot
car::influencePlot(all_mormon_model2)

# Measure of fit
(rmse <- sqrt(mean((all_mormon_model2$fitted.values - all_data$leave)^2)))


### All Mormon Model 3: With recoded marital status binning separated, divorced, + widowed
#Summary
summary(all_mormon_model3)

# Cooks D + outlier plot
car::influencePlot(all_mormon_model3)

# Measure of fit
(rmse <- sqrt(mean((all_mormon_model3$fitted.values - all_data$leave)^2)))


##### Step 4: Predicting Probabilities of Leaving #####

# probability of leaving if you are non-white
yrs_until_marriage <- seq(0,1,1)
len_yrs <- length(yrs_until_marriage)

new_data <- data.frame(
  age_rc = rep(mean(all_data$age_rc),len_yrs),
  educ_rc = rep(mean(all_data$educ_rc, na.rm = T),len_yrs),
  non_white = yrs_until_marriage,
  SEX = rep(mean(all_data$SEX),len_yrs),
  marital_rc = rep(1,len_yrs)
)

new_data$predicted <- predict(all_mormon_model, newdata = new_data, type = "response")
print(new_data$predicted)


# probability of leaving with education
yrs_until_marriage <- seq(1,4,1)
len_yrs <- length(yrs_until_marriage)

new_data <- data.frame(
  age_rc = rep(mean(all_data$age_rc),len_yrs),
  educ_rc = yrs_until_marriage,
  non_white = rep(mean(all_data$non_white, na.rm = T),len_yrs),
  SEX = rep(mean(all_data$SEX),len_yrs),
  marital_rc = rep(1,len_yrs)
)

new_data$predicted <- predict(all_mormon_model, newdata = new_data, type = "response")
print(new_data$predicted)


# probability of leaving with sex
yrs_until_marriage <- c(0,1)
len_yrs <- length(yrs_until_marriage)

new_data <- data.frame(
  age_rc = rep(mean(all_data$age_rc),len_yrs),
  educ_rc = rep(mean(all_data$educ_rc, na.rm = T),len_yrs),
  non_white = rep(mean(all_data$non_white, na.rm = T),len_yrs),
  SEX = yrs_until_marriage,
  marital_rc = rep(1,len_yrs)
)

new_data$predicted <- predict(all_mormon_model, newdata = new_data, type = "response")
print(new_data$predicted)


#probability of leaving by education moderated by sex
educ <- c(seq(1,4,1),seq(1,4,1))
sex <- c(rep(1,4),rep(0,4))
len_yrs <- length(yrs_until_marriage)

new_data <- data.frame(
  age_rc = rep(mean(all_data$age_rc),len_yrs),
  educ_rc = educ,
  non_white = rep(mean(all_data$non_white, na.rm = T),len_yrs),
  SEX = sex,
  marital_rc = rep(1,len_yrs)
)

new_data$predicted <- predict(all_mormon_model, newdata = new_data, type = "response")
print(new_data$predicted)

# Probability of Leaving by marital status
# 1 = other 2= married 3=divorced 4=single
mar <- c(1,2,3,4)
len_yrs <- length(mar)

new_data <- data.frame(
  age_rc = rep(mean(all_data$age_rc),len_yrs),
  educ_rc = rep(mean(all_data$educ_rc, na.rm = T),len_yrs),
  non_white = rep(mean(all_data$non_white, na.rm = T),len_yrs),
  SEX = rep(mean(all_data$SEX),len_yrs),
  marital_rc = mar
)

new_data$predicted <- predict(all_mormon_model2, newdata = new_data, type = "response")
print(new_data$predicted)


# Probablity of leaving by age
yrs_until_marriage <- seq(1,13,1)
len_yrs <- length(yrs_until_marriage)

new_data <- data.frame(
  age_rc = yrs_until_marriage,
  educ_rc = rep(mean(all_data$educ_rc, na.rm = T),len_yrs),
  non_white = rep(mean(all_data$non_white, na.rm = T),len_yrs),
  SEX = rep(mean(all_data$SEX),len_yrs),
  marital_rc = rep(4,len_yrs)
)
new_data$predicted <- predict(all_mormon_model2, newdata = new_data, type = "response")
print(new_data$predicted)



# Probablity of leaving by marital status where divorced are combined with separated and widowed
mar <- c(1,2,3,4)
len_yrs <- length(mar)

new_data <- data.frame(
  age_rc = rep(mean(all_data$age_rc),len_yrs),
  educ_rc = rep(mean(all_data$educ_rc, na.rm = T),len_yrs),
  non_white = rep(mean(all_data$non_white, na.rm = T),len_yrs),
  SEX = rep(mean(all_data$SEX),len_yrs),
  marital_rc2 = mar
)
new_data$predicted <- predict(all_mormon_model3, newdata = new_data, type = "response")
print(new_data$predicted)

# Probablity of leaving by age
yrs_until_marriage <- seq(1,13,1)
len_yrs <- length(yrs_until_marriage)

new_data <- data.frame(
  age_rc = yrs_until_marriage,
  educ_rc = rep(mean(all_data$educ_rc, na.rm = T),len_yrs),
  non_white = rep(mean(all_data$non_white, na.rm = T),len_yrs),
  SEX = rep(mean(all_data$SEX),len_yrs),
  marital_rc2 = rep(4,len_yrs)
)
new_data$predicted <- predict(all_mormon_model3, newdata = new_data, type = "response")
print(new_data$predicted)
