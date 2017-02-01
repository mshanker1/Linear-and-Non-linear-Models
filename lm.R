# This program is to demonstrate R commands for multiple linear regression
# read in a data file
# reference http://www.statmethods.net/stats/regression.html
# https://github.com/kabacoff/RiA2/blob/master/Ch08%20Regression.R
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

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# Regression Diagnostics
# Assume that we are fitting a multiple linear regression
library(car)
fit <- lm(Sales ~ Accts+Adver)
# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-p-1) 
cutoff <- 4/((nrow(SalesPerf)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)

# compare models
fit1 <- lm(Sales ~ Accts+Adver+Change)
fit2 <- lm(Sales ~ Accts+Adver)
anova(fit1, fit2)

# K-fold cross-validation
library(DAAG)
cv.lm(data=SalesPerf, fit, m=3) # 3 fold cross-validation

# Stepwise Regression
library(MASS)
fit <- lm(Sales ~ Time+MktPoten+MktShare+Adver+Change+Accts+WkLoad)
step <- stepAIC(fit, direction="both")
step$anova # display results

# All Subsets Regression
library(leaps)
attach(SalesPerf)
leaps<-regsubsets(Sales ~ Time+MktPoten+MktShare+Adver+Change+Accts+WkLoad,data=SalesPerf,nbest=1)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 
library(car)
subsets(leaps, statistic="rsq")

#Other options for plot( ) are bic, Cp, and adjr2. Other options for plotting with 
# subset( ) are bic, cp, adjr2, and rss.

# Calculate Relative Importance for Each Predictor
# See help(calc.relimp) for details on the four measures of relative importance provided.
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fit, b = 1000, type = c("lmg", 
                                            "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result
