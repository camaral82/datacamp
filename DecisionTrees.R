# 13/10/2020 - martedì - Nunatac

# DECISION TREE

df <- read.csv("~/Carlos/2. DATACAMP selfstudy/Supervised Learning in R/loans.csv", 
                  stringsAsFactors=TRUE)

"Building a simple decision tree"

# Load the rpart package
library(rpart)

# Convert the default to a factor - create the 'outcome'
loans <- df[]
loans$outcome <- factor(loans$default, 
                        levels = c(0, 1), 
                        labels = c("default", "repaid"))

del <- c("keep", "rand", "default")

loans[del] <- NULL
str(loans)

# Build a lending model predicting loan outcome versus loan amount and credit score
loan_model <- rpart(outcome ~ loan_amount + credit_score , 
                    data = loans, 
                    method = "class", 
                    control = rpart.control(cp = 0))


# Make a prediction for someone with good credit
predict(loan_model, good_credit, type = "class")

# Make a prediction for someone with bad credit
predict(loan_model, bad_credit, type="class")



"Visualizing classification trees"
# Examine the loan_model object
loan_model

# Load the rpart.plot package
library(rpart.plot)

# Plot the loan_model with default settings
rpart.plot(loan_model)

# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, 
           box.palette = c("red", "green"), 
           fallen.leaves = TRUE)




"Why do some branches split?
A classification tree grows using a divide-and-conquer process. 
Each time the tree grows larger, 
it splits groups of data into smaller subgroups, 
creating new branches in the tree.

Given a dataset to divide-and-conquer,
which groups would the algorithm prioritize to split first?
The group it can split to create the greatest improvement 
in subgroup homogeneity
Divide-and-conquer always looks to create the split 
resulting in the greatest improvement to purity."


"Creating random test datasets"

# Determine the number of rows for training
n <- round(nrow(loans)*0.75)
n

# Create a random sample of row IDs
sample_rows <- sample(nrow(loans), n)

# Create the training dataset
loans_train <- loans[sample_rows,]

# Create the test dataset
loans_test <- loans[ -sample_rows, ]


"Building and evaluating a larger tree"
# Grow a tree using all of the available applicant data
loan_model <- rpart(outcome ~ . , 
                    data = loans_train, 
                    method = "class", 
                    control = rpart.control(cp = 0))
loan_model

# Make predictions on the test dataset
loans_test$pred <- predict(loan_model, loans_test, type="class")

# Examine the confusion matrix
table(loans_test$pred, loans_test$outcome)

# Compute the accuracy on the test dataset
mean(loans_test$pred == loans_test$outcome)
  
# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, 
           box.palette = c("red", "green"), 
           fallen.leaves = TRUE)


"Preventing overgrown trees"
"pre-prunning method"
# Grow a tree with maxdepth of 6
loan_model <- rpart(outcome ~ . , 
                    data = loans_train, 
                    method = "class", 
                    control = rpart.control(cp = 0, maxdepth=6))

# Make a class prediction on the test set
loans_test$pred <- predict(loan_model, loans_test,  type="class")

# Compute the accuracy of the simpler tree
mean(loans_test$pred == loans_test$outcome)

# Swap maxdepth for a minimum split of 500 
loan_model <- rpart(outcome ~ ., data = loans_train, 
                    method = "class", 
                    control = rpart.control(cp = 0, minsplit=500))

# Run this. How does the accuracy change?
loans_test$pred <- predict(loan_model, loans_test, type = "class")
mean(loans_test$pred == loans_test$outcome)

"Post-Pruning"

# Grow an overly complex tree
loan_model <- rpart(outcome ~ . , 
                                  data = loans_train, 
                                  method = "class", 
                                  control = rpart.control(cp = 0))

# Examine the complexity plot
plotcp(loan_model)

# Prune the tree
loan_model_pruned <- prune(loan_model, cp = 0.0014)

# Compute the accuracy of the pruned tree
loans_test$pred <- predict(loan_model_pruned, loans_test, type = "class")
mean(loans_test$pred == loans_test$outcome)



"RANDOM FORESTS

Groups of classification trees can be combined into an ensemble that 
generates a single prediction by allowing the trees to 'vote' on the outcome.

Why might someone think that this could result in
more accurate predictions than a single tree?

The diversity among the trees may lead it to discover more subtle 
The teamwork-based approach of the random forest 
may help it find important trends a single tree may miss.
"
# Load the randomForest package
library(randomForest)

# Build a random forest model
loan_model <- randomForest(outcome ~ . , data = loans_train)

# Compute the accuracy of the random forest
loans_test$pred <- predict(loan_model, loans_test, type="class")
mean(loans_test$pred == loans_test$outcome)

