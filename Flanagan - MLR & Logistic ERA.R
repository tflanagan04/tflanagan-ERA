# Reading in file
data <- read.csv("C:\\Users\\flana\\Downloads\\Pitching.csv")

# Exploratory Analysis
head(data)
summary(data)
names(data)

# Checking for nulls and duplicates
colSums(is.na(data))
sum(duplicated(data))

# Creating numerical field (with innings filter), finding correlation matrix
# Filter is for anyone with more than 80 innings pitched
data_num <- data[data$IPouts > 240, sapply(data, is.numeric)]
cor(data_num)

# Numerical field (with innings filter) with first cut off
# Why 1977? This is when the first wave of sabermetrics were available, albeit they were not widely used
data_num_1977 <- data[data$yearID >= 1977 & data$IPouts > 240, sapply(data, is.numeric)]
cor(data_num_1977)

# Numerical field with a year cut off of 2000 and same innings filter
# Why 2000? The start of a new century of MLB, and the new wave of baseball was incoming
data_num_2000 <- data[data$yearID >= 2000 & data$IPouts > 240, sapply(data, is.numeric)]
cor(data_num_2000)

# Numerical field with a year cutoff of 2015 and same innings filter
# Why 2015? Beginning of Statcast Era in MLB
data_num_2015 <- data[data$yearID >= 2015 & data$IPouts > 240, sapply(data, is.numeric)]
cor(data_num_2015)

# Number of observations in each filtered set
nrow(data_num)
nrow(data_num_1977)
nrow(data_num_2000)
nrow(data_num_2015)

# Splitting up data
# FIRST MODEL - using the entire data set (1871-2024)
set.seed(123)

# Cleaning data (there were 86 HBP nulls and 8 HR nulls)
data_num_clean <- data_num[complete.cases(data_num[, c("SO", "BB", "HBP", "HR")]), ]

# 70% of the data into training split, 30% to the testing split
index <- sample(1:nrow(data_num_clean), 0.7 * nrow(data_num_clean))
train <- data_num_clean[index, ]
test <- data_num_clean[-index, ]

nrow(train)
nrow(test)

# Training model using the four metrics and its descriptive summary
first_model <- lm(ERA ~ SO + BB + HBP + HR, data = train)
summary(first_model)

# Displays the testing results with quartiles and more info
first_pred <- predict(first_model, newdata = test)
summary(first_pred)

# Calculating residuals
first_residuals <- test$ERA - first_pred

# Displaying actual and predicted values along with the differences (residuals)
first_results <- data.frame(Actual = test$ERA, Predicted = first_pred, Residual = first_residuals)
head(first_results, 10)

# Calculating RSE and displaying it
RSE <- sqrt(mean((test$ERA - first_pred)^2, na.rm=TRUE))
RSE

# SECOND MODEL - 1977 cut off (1977-2024)
set.seed(123)
data_num_1977_clean <- data_num_1977[complete.cases(data_num_1977[, c("SO", "BB", "HBP", "HR")]), ]
index_2 <- sample(1:nrow(data_num_1977_clean), 0.7 * nrow(data_num_1977_clean))

# Creating new training and testing sets
train_2 <- data_num_1977_clean[index_2, ]
test_2 <- data_num_1977_clean[-index_2, ]

nrow(data_num_1977_clean)
nrow(train_2)
nrow(test_2)

second_model <- lm(ERA ~ SO + BB + HBP + HR, data = train_2)
summary(second_model)

second_pred <- predict(second_model, newdata = test_2)
summary(second_pred)

second_residuals <- test_2$ERA - second_pred

second_results <- data.frame(Actual = test_2$ERA, Predicted = second_pred, Residual = second_residuals)
head(second_results, 10)

RSE_2 <- sqrt(mean((test_2$ERA - second_pred)^2, na.rm=TRUE))
RSE_2

# THIRD MODEL - 2000 cut off (2000-2024)
set.seed(123)
data_num_2000_clean <- data_num_2000[complete.cases(data_num_2000[, c("SO", "BB", "HBP", "HR")]), ]
index_3 <- sample(1:nrow(data_num_2000_clean), 0.7 * nrow(data_num_2000_clean))

train_3 <- data_num_2000_clean[index_3, ]
test_3 <- data_num_2000_clean[-index_3, ]

nrow(data_num_2000_clean)
nrow(train_3)
nrow(test_3)

third_model <- lm(ERA ~ SO + BB + HBP + HR, data = train_3)
summary(third_model)

third_pred <- predict(third_model, newdata = test_3)
summary(third_pred)

third_residuals <- test_3$ERA - third_pred

third_results <- data.frame(Actual = test_3$ERA, Predicted = third_pred, Residual = third_residuals)
head(third_results, 10)

RSE_3 <- sqrt(mean((test_3$ERA - third_pred)^2, na.rm=TRUE))
RSE_3

# FOURTH MODEL - 2015 cut off (2015-2024)
set.seed(123)
data_num_2015_clean <- data_num_2015[complete.cases(data_num_2015[, c("SO", "BB", "HBP", "HR")]), ]
index_4 <- sample(1:nrow(data_num_2015_clean), 0.7 * nrow(data_num_2015_clean))

train_4 <- data_num_2015_clean[index_4, ]
test_4 <- data_num_2015_clean[-index_4, ]

nrow(data_num_2015_clean)
nrow(train_4)
nrow(test_4)

fourth_model <- lm(ERA ~ SO + BB + HBP + HR, data = train_4)
summary(fourth_model)

fourth_pred <- predict(fourth_model, newdata = test_4)
summary(fourth_pred)

fourth_residuals <- test_4$ERA - fourth_pred

fourth_results <- data.frame(Actual = test_4$ERA, Predicted = fourth_pred, Residual = fourth_residuals)
head(fourth_results, 10)

RSE_4 <- sqrt(mean((test_4$ERA - fourth_pred)^2, na.rm=TRUE))
RSE_4

# Since the fourth model was most accurate (highest R-Squared and lowest RSE)...
# We will use that numerical field to further analyze what we want

# We now know that what a pitcher has DIRECT control over, since the Statcast Era,
# explains 51.97% of the variation in ERA. This makes sense because there are outside
# factors that affect ERA output. Now, let's try and find what outside factors are most important.

# FIFTH MODEL - 2015 cut off with updates (2015-2024)
# Using the same numerical field and index

# Home runs subtracted from hits so HR is not double counted
data_num_2015_clean$H_minus_HR <- data_num_2015_clean$H - data_num_2015_clean$HR
index_5 <- sample(1:nrow(data_num_2015_clean), 0.7 * nrow(data_num_2015_clean))

train_5 <- data_num_2015_clean[index_5, ]
test_5 <- data_num_2015_clean[-index_5, ]

nrow(data_num_2015_clean)
nrow(train_5)
nrow(test_5)

# Refreshing on what is in data set
names(data_num_2015_clean)

# Adding Hits and Opponent's Batting Average to model
fifth_model <- lm(ERA ~ SO + BB + HBP + HR + H_minus_HR + BAOpp, data = train_5)
summary(fifth_model)

# The large estimate for BAOpp is this:
# For every .010 increase in BAOpp, ERA increases by .323
# So, someone with a 1.000 BAOpp would have an ERA of 32.32 (and you can move the decimal accordingly)

fifth_pred <- predict(fifth_model, newdata = test_5)
summary(fifth_pred)

fifth_residuals <- test_5$ERA - fifth_pred

fifth_results <- data.frame(Actual = test_5$ERA, Predicted = fifth_pred, Residual = fifth_residuals)
head(fifth_results, 10)

RSE_5 <- sqrt(mean((test_5$ERA - fifth_pred)^2, na.rm=TRUE))
RSE_5

# SIXTH MODEL - Logistic Regression (2015-2024)

# Saying that our "1" or "positive" outcome is an ERA less than 4.00 since lower ERA is better
train_5$ERA_under_4 <- ifelse(train_5$ERA < 4, 1, 0)
test_5$ERA_under_4 <- ifelse(test_5$ERA < 4, 1, 0)

# Creating model with parameters of previous model but with logistic framework instead of MLR
logistic_model <- glm(ERA_under_4 ~ SO + BB + HBP + HR + H_minus_HR + BAOpp, data = train_5, family = 'binomial')
summary(logistic_model)

p_lessthan <- predict(logistic_model, newdata = test_5, type = 'response')
sixth_class <- ifelse(p_lessthan > .5, 1, 0)
sixth_pred <- factor(sixth_class, levels = c(0, 1))

sixth_results <- data.frame(Prob_lessthan = round(p_lessthan, 3), Predicted = sixth_pred, Actual = test_5$ERA)
head(sixth_results, 25)

# 2x2 confusion matrix displaying true positives, true negatives, false positives, and false negatives
cm <- table(Actual = test_5$ERA_under_4, Predicted = sixth_pred)
cm

accuracy <- sum(diag(cm)) / sum(cm)
round(accuracy, 4)

# MLR Adjusted R-Squared Value comparisons (in order of models 1-5)
round(summary(first_model)$adj.r.squared, 4) # 1871-2024
round(summary(second_model)$adj.r.squared, 4) # 1977-2024
round(summary(third_model)$adj.r.squared, 4) # 2000-2024
round(summary(fourth_model)$adj.r.squared, 4) # 2015-2024
round(summary(fifth_model)$adj.r.squared, 4) # 2015-2024 (with updated metrics)

# The upward trend from the first to fourth models shows the relationship in how 
# stressed each metrics has become in relation to successful output. The adding 
# metrics for the fifth model is done to increase predicting power

# MLR RSE Value Comparisons
round(RSE, 4) # 1871-2024
round(RSE_2, 4) # 1977-2024
round(RSE_3, 4) # 2000-2024
round(RSE_4, 4) # 2015-2024
round(RSE_5, 4) # 2015-2024 (with updated metrics)

# The downward trend (and therefore increased consistency) as the years get closer 
# to modern day reflects the same relationship as shown with the R-Squared values. The 
# extreme drop off to .4332 is because of the updated model, while the downward trend 
# in 1-4 is an indicator on the philosophy surrounding the game.

# Sixth Model Eval
# Refresher on the accuracy
    # Accuracy represents how well the logistic model can predict if a pitcher has an 
    # ERA under 4 with the metrics given (same model outline as Model #5)

round(accuracy, 4)

# This means that the logistic model predicts if a pitcher has an ERA under 4 
# 84.65% of the time correctly. 

# QUESTIONS #

## Why was the 4.00 ERA threshold used? ## 
# Being below a 4.00 ERA is a good indicator of solid or higher end output. With the model
# being at 84%, this is a good sign that these six metrics are key in evaluating 
# if a pitcher is successful

## Why were the six metrics not used in the other models? ##
# This was to communicate how the importance of these metrics have been stressed
# through the years. For example, in 1924, only 2 pitchers recorded more than 100 strikeouts
# while in 2024, 8 recorded more than 200 strikeouts. The values show that as we get into
# the Statcast Era these metrics have stronger ties to ERA output. 
# However, the final model could not only have a .5197 adjusted r-squared value, so hits
# and BAOpp were added. This way, the message of what a pitcher can control being more 
# important gets across and we can also see what other metrics add predictive value.

# FINAL TAKEAWAY:
# With evidence, just because a pitcher controls everything they can well does not mean they will have success.
