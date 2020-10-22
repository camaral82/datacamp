#09/10/2020

"Building simple logistic regression models"
donors <- read.csv("~/Carlos/2. DATACAMP selfstudy/Supervised Learning in R/donors.csv")

# Examine the dataset to identify potential independent variables
str(donors)

# Explore the dependent variable
table(donors$donated)

# Build the donation model
donation_model <- glm(donated ~ bad_address+interest_religion+interest_veterans, 
                      data = donors, family = "binomial")

# Summarize the model results
summary(donation_model)

"By default, predict() outputs predictions in terms of log odds 
unless type = 'response' is specified. 
This converts the log odds to probabilities."

"Making a binary prediction"
# Estimate the donation probability
donors$donation_prob <- predict(donation_model, type = "response")

names(donors)
# Find the donation probability of the average prospect
mean(donors$donated)

# Predict a donation if probability of donation is greater 
#than average (0.0504)
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donation_pred == donors$donated)


"What would the accuracy have been if a model had simply predicted 
'no donation' for each person?"
donors$donation_pred_no <- ifelse(donors$donation_prob < 0.0504, 0, 1)

mean(donors$donation_pred_no == donors$donated)


"Calculating ROC Curves and AUC"
# Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)
"Based on this visualization, the model isn't doing much better 
than baseline- a model doing nothing but making predictions at random.
"


"Coding categorical features
Sometimes a dataset contains numeric values 
that represent a categorical feature."

# Convert the wealth rating to a factor
donors$wealth_levels <- factor(donors$wealth_rating, 
                               levels = c(0, 1, 2, 3), 
                               labels = c("Unknown", "Low", "Medium", "High"))

# Use relevel() to change reference category
donors$wealth_levels <- relevel(donors$wealth_levels, ref = "Medium")

# See how our factor coding impacts the model
summary(glm(donated ~ wealth_levels, data=donors, family="binomial"))



"Handling missing data"
# Find the average age among non-missing values
summary(donors$age)

# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age), 
                             round(mean(donors$age, na.rm=TRUE), 2), 
                             donors$age)

# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)


"Building a more sophisticated model
One of the best predictors of future giving is a history of
recent, frequent, and large gifts. 
In marketing terms, this is known as R/F/M:

*Recency
*Frequency
*Money
Donors that haven't given both recently and frequently may be 
especially likely to give again; in other words, the combined 
impact of recency and frequency may be greater than the sum of the separate effects.

Because these predictors together have a greater impact on the 
dependent variable, their joint effect must be modeled as an interaction"


# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ money+recency*frequency, data=donors, family="binomial")

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model,data=donors, type="response")

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated, rfm_prob)
plot(ROC, col = "red")
auc(ROC)


"Building a stepwise regression model
In the absence of subject-matter expertise, 
stepwise regression can assist with the search for the most 
important predictors of the outcome of interest.
"

# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, 
                   scope = list(lower = null_model, upper = full_model), 
                   direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model, data=donors, type="response")

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)
