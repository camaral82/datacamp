#09/10/2020

#NAIVE BAYES
library(naivebayes)

"Computing probabilities"
# Compute P(A) 
str(where9am)
unique(where9am$location)
p_A <- nrow(subset(where9am, location=="office"))/nrow(where9am)
p_A

# Compute P(B)
unique(where9am$daytype)
p_B <- nrow(subset(where9am, daytype=="weekday"))/nrow(where9am)
p_B

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, (daytype=="weekday" & 
                                 location=="office")
                    ))/nrow(where9am)
p_AB

# Compute P(A | B) and print its value
p_A_given_B <- p_AB/p_B
p_A_given_B


"A simple Naive Bayes location model
the probability that Brett is at work or at 
home at 9am is highly dependent on whether 
it is the weekend or a weekday."
# Load the naivebayes package
library(naivebayes)

# Build the location prediction model
locmodel <- naive_bayes(location~daytype, data = where9am)

# Predict Thursday's 9am location
thursday9am #daytype - weekday
str(thursday9am) #data.frame, factor w 2 levels weekday weekend
predict(locmodel, newdata = thursday9am)

# Predict Saturdays's 9am location
saturday9am #weekend
predict(locmodel, saturday9am)


"Examining 'raw' probabilities"
# Examine the location prediction model
locmodel

# Obtain the predicted probabilities for Thursday at 9am
predict(locmodel, thursday9am , type = "prob")

# Obtain the predicted probabilities for Saturday at 9am
predict(locmodel, saturday9am , type = "prob")



"Understanding independence
Understanding the idea of event independence will become important 
as you learn more about how 'naive' Bayes got its name. 
Which of the following is true about independent events?

Knowing the outcome of one event does not help predict the other.
One event is independent of another if knowing one doesn't give you 
information about how likely the other is. 
For example, knowing if it's raining in New York 
doesn't help you predict the weather in San Francisco. 
The weather events in the two cities are independent of each other.
"



"The Naive Bayes algorithm got its name because it makes a 
'naive' assumption about event independence.

What is the purpose of making this assumption?
The joint probability of independent events can be computed much 
more simply by multiplying their individual probabilities.
"



"A more sophisticated location model
The locations dataset records Brett's location every hour for 13
weeks. Each hour, the tracking information includes the daytype
(weekend or weekday) as well as the hourtype (morning, afternoon,
evening, or night)."
# Build a NB model of location
str(locations)
locmodel <- naive_bayes(location~daytype+hourtype, data=locations)

# Predict Brett's location on a weekday afternoon
str(weekday_afternoon)
predict(locmodel, weekday_afternoon)

# Predict Brett's location on a weekday evening
predict(locmodel, weekday_evening)


"Preparing for unforeseen circumstances
While Brett was tracking his location over 13 weeks, he never went
into the office during the weekend. Consequently, the joint
probability of P(office and weekend) = 0.
Explore how this impacts the predicted probability that Brett may go
to work on the weekend in the future. Additionally, you can see how
using the Laplace correction will allow a small chance for these
types of unforeseen circumstances.
"
# Observe the predicted probabilities for a weekend afternoon
predict(locmodel, weekend_afternoon, type="prob")

# Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location~daytype+hourtype, 
                         data=locations, laplace=1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2, weekend_afternoon, type="prob")

"Adding the Laplace correction allows for the small chance that
Brett might go to the office on the weekend in the future.
The small probability added to every outcome ensures that they are 
all possible even if never previously observed."


"Handling numeric predictors
Numeric data is often binned before it is used with Naive Bayes.
ex
age values recoded as 'child' or 'adult' categories
geographic coordinates recoded into geographic regions (West, East, etc.)
test scores divided into four groups by percentile"



