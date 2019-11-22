# Este é um trabalho de conclusão do módulo de Modelos de Regressão da especialização
# em Data Science oferecida pela Johns Hopkins University através da plataforma 
# Cousera.

# You work for Motor Trend, a magazine about the automobile industry. 
# Looking at a data set of a collection of cars, they are interested in exploring 
# the relationship between a set of variables and miles per gallon (MPG) (outcome). 
# They are particularly interested in the following two questions:
#   
# "Is an automatic or manual transmission better for MPG"
# "Quantify the MPG difference between automatic and manual transmissions"

# Criteria
# 
# Did the student interpret the coefficients correctly?
# Did the student do some exploratory data analyses?
# Did the student fit multiple models and detail their strategy for model selection?
# Did the student answer the questions of interest or detail why the question(s) is (are) not answerable?
# Did the student do a residual plot and some diagnostics?
# Did the student quantify the uncertainty in their conclusions and/or perform an inference correctly?
# Was the report brief (about 2 pages long) for the main body of the report and no longer than 5 with supporting appendix of figures?
# Did the report include an executive summary?
# Was the report done in Rmd (knitr)?

library(dplyr)
data("mtcars")
str(mtcars)
cars <- mtcars

# Transforming the variables in factors
# 0 = automatic, 1 = manual
cars$am <- factor(cars$am, levels = c(0,1))
str(cars)
pairs(cars)

fitall <- lm(mpg ~ factor(am) + . -1, data=cars)
plot(fitall$residuals)
summary(fitall)

plot(x=cars$am, y=cars$mpg)
fit1 <- lm (mpg ~ factor(am) -1, data = cars)
plot(fit1$residuals)

fit2 <- lm (mpg ~ factor(am) + wt -1, data = cars)
summary(fit2)
plot(fit2$residuals)

fit3 <- lm (mpg ~ factor(am) + wt + hp -1, data = cars)
plot(fit3$residuals)
summary(fit3)

fit4 <- lm (mpg ~ factor(am) + wt + hp + disp -1, data = cars)
plot(fit4$residuals)
summary(fit4)

fit5 <- lm (mpg ~ factor(am) + wt + drat -1, data = cars)
plot(fit5$residuals)
summary(fit5)

anova(fit1,fit2,fit3,fit4,fit5, fitall)

