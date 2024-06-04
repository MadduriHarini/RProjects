install.packages("tidyverse")
library(tidyverse)
# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# if you run this on your local machine, then can remove tar = "internal" 
untar("lax_to_jfk.tar.gz", tar = "internal")

# read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))

# ********* Simple Linear Regression *************
# Define dataset with just AA as the Reporting_Airline
aa_delays <- sub_airline %>%
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")

head(aa_delays)
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
summary(linear_model)
# Input data we use to predict
new_depdelay <- data.frame(
  DepDelayMinutes = c(12, 19, 24))

# Predict the data points
pred <- predict(linear_model, newdata = new_depdelay, interval = "confidence")
pred
linear_model$coefficients
## Question 1a) create a linear function with carrier delay as predictor variable
# and Arrdelymin as response variable
linear_model2 <- lm(ArrDelayMinutes ~ CarrierDelay, 
                    data = aa_delays)
# 1b) Find the slopes and intercepts (coefficients) of the model
linear_model2$coefficients
# 1c) What is the equation of the predicted line? 
# using X and Y  
Yhat = 35.11 + 0.7032 * X

ArrDelayMinutes = 35.11 + 0.7032 *CarrierDelay         
# ********** Multiple Linear Regression **********************
mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delays)

summary(mlr)
mlr$coefficients
# Question 2a) Create and train a multiple linear regression model "mlr2" where the response variable is ArrDelayMinutes, and the predictor variable is 'DepDelayMinutes', 'LateAircraftDelay' and 'CarrierDelay'.
mlr2 <- lm(
  ArrDelayMinutes ~ DepDelayMinutes + 
    LateAircraftDelay + CarrierDelay, 
  data = aa_delays)

summary(mlr2) 
# 2b) Find the coefficients of the model
mlr2$coefficient 
# 2c) Using the fitted model, mlr2, what are the predicted values for the following new data points?
# New data points
DepDelayMinutes <- c(10, 20, 30)
LateAircraftDelay <- c(20, 60, 30)
new_multidelay <- data.frame(DepDelayMinutes, LateAircraftDelay)
pred <- predict(mlr, 
                newdata = new_multidelay, 
                interval = "confidence")  
pred
# ****************** Assessing Models Visually ************************
# best way to assess models by visualization
ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")
# Question 3a) Create a regression plot of "CarrierDelay" and "ArrDelayMinutes" using "aa_delays" dataset
ggplot(
  aa_delays, 
  aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")
# Question 3b) Given the regression plots above is "DepDelayMinutes" or "CarrierDelay" more strongly correlated with "ArrDelayMinutes". Use the method "cor()" to verify your answer
# The variable "DepDelayMinutes" has a stronger correlation with "ArrDelayMinutes", it is approximately 0.871  compared to "CarrierDelay" which is approximately 0.624. You can verify it using the following commands:

cor(aa_delays$DepDelayMinutes, 
    aa_delays$ArrDelayMinutes)
cor(aa_delays$CarrierDelay, 
    aa_delays$ArrDelayMinutes)
##### Residual Plot
aa_delays<- sub_airline %>%
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")
score_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
aa_delays$predicted <- predict(score_model)

ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = DepDelayMinutes, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look
# We can see from this residual plot that the residuals are not randomly spread around the x-axis, which leads us to believe that maybe a non-linear model is more appropriate for this data
ggplot(lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)) +
  geom_point(aes(x=DepDelayMinutes, y=.resid))
## Other Diagnostic plots
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
plot(linear_model)
set.seed(20)
x <- seq(from=0, to=20, by=0.1)

# value to predict (y):
y <- 500 + 0.4 * (x-10)^3

# some noise is generated and added to the real signal (y):
noise <- rnorm(length(x), mean=10, sd=80)
noisy.y <- y + noise
# fit linear model
ggplot(data=NULL,aes(x, noisy.y)) + 
  geom_point() + 
  geom_smooth(method = "lm")
ggplot(data=NULL,aes(x, noisy.y)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 5))
## Ploynomial 2nd order
time <- 6:19
temp <- c(4,6,7,9,10,11,11.5,12,12,11.5,11,10,9,8)

ggplot(data = NULL, aes(time, temp)) + 
  geom_point() 
polyfit2 <- lm(temp ~ poly(time, 2, raw = TRUE))

summary(polyfit2)
ggplot(data = NULL, aes(time, temp)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))
# Question 4a) Create a 4th order polynomial model with the variables time and temp from above and display the summary of the model.
polyfit4 <- lm(temp ~ poly(tim#e, 4, raw = TRUE))
# print results
summary(polyfit4)
# 4b) Using the predicted coefficients from the summary output for the 4th order model, write down the model equation.
temp = 0.9580 -1.683 * time 
+ 0.5770 * time^2 
- 0.03971 * time^3 
+ 0.0007906 * time^4
### ************ Assessing the Model ****************
# Model1 - Simple linear regression
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, aa_delays)
mse <- mean(linear_model$residuals^2)
mse
rmse <- sqrt(mse)
rmse
summary(linear_model)$r.squared
# Model2 - Multiple linear regression
mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delays)
mse_mlr <- mean(mlr$residuals^2)
mse_mlr
rmse_mlr <- sqrt(mse_mlr)
rmse_mlr
summary(mlr)$r.squared
# Model3 - Polynomial linear regression
poly_reg <- lm(ArrDelayMinutes ~ poly(DepDelayMinutes, 3), data = aa_delays)
mse_poly <- mean(poly_reg$residuals^2)
mse_poly
rmse_poly <- sqrt(mse)
rmse_poly
summary(poly_reg)$r.squared
# ***************** Predction and decision making
# For example we want to predict the score model we created in a previous section
head(predict(score_model))
