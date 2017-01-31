# This program is to demonstrate R commands for multiple linear regression
# read in a data file
# reference http://www.statmethods.net/stats/regression.html
# install.packages(c("car", "gvlma", "MASS", "leaps"))       #
#------------------------------------------------------------#
install.packages(c("car", "gvlma", "MASS", "leaps"))

# import the file
setwd("/Users/mshanker/Downloads/RCode/Linear-and-Non-linear-Models")
list.files(".")
SalesPerf <- read.csv("salesperfdata.csv")
summary(SalesPerf)
SalesPerf <- SalesPerf[,-9] # remove the last column as it contains NAs.
attach(SalesPerf) # attach the dataset so we can use column names

#perform preliminary analysis
cor(SalesPerf) # find the correlation between variables
fit <- lm(Sales~Accts,SalesPerf) # fit a simple linear regression model
summary(fit) # summary of the fit
plot(Accts,Sales, 
     main = "Plot of Accts versus Sales",
     xlab = "Accts",
     ylab = "Sales"
     )
abline(fit) # plot the regression line on the current plot; make sure your x and y are conistent
plot(fit) # regression plots
abline(fit) # supposed to plot the regression line on the current plot
fit$coefficients # print out the coefficients, alternatively
coef(fit)
confint(fit,level=0.90) # print out a 90% CI for the fit
fitted(fit) # prints out fitted values
resid(fit) # prints out residual values
anova(fit)
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

