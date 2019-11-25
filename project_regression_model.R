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
library(car)
data("mtcars")
str(mtcars)
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Automatic", "Manual")
cars <- mtcars

# Transforming the variables in factors
# 0 = automatic, 1 = manual
#cars$am <- factor(cars$am, levels = c(0,1))
str(cars)
cor(mtcars)[1,]
pairs(cars)

fit0 <- lm(mpg ~ factor(am), data=cars)
tcritical <- qt(0.975, fit0$df)
print(paste("Critical t value at 95% of confidence to reject null hypothesis :", round(tcritical, 3)))
summary(fit0)

#fitall <- lm(mpg ~ factor(am) + cyl + disp + hp + drat + wt + qsec + factor(vs) + factor(gear) + factor(carb), data=cars)
fitall <- lm(mpg ~ ., data=cars)
summary(fitall)$adj.r.squared

alias(fitall)
sqrt(vif(fitall))

fit1 <- lm(mpg ~ factor(am) + wt, data=cars)
vif(fit1)
summary(fit1)$adj.r.squared

fit2 <- lm(mpg ~ factor(am) + wt + hp, data=cars)
vif(fit2)
summary(fit2)$adj.r.squared

# A inclusão do preditor disp não melhorou o desempenho 
fit3 <- lm(mpg ~ factor(am) + wt + hp + disp, data=cars)
vif(fit3)
summary(fit3)$adj.r.squared


fit4 <- lm(mpg ~ factor(am) + wt + hp + cyl, data=cars)
vif(fit4)
summary(fit4)$adj.r.squared
tcritical <- qt(0.975, fit4$df)


# Since disp and HP seens to be very correlated to Disp and Cyl they will be 
# removed from the model.
# Also, the variable carb look very inflated by the other, suggesting noise to the model

# fit1 <- lm(mpg ~ factor(am) + hp + drat + wt  + factor(vs) + factor(gear) , data=cars)
# vif(fit1)
# summary(fit1)
# 
# # Predictor gear seens to be inflated by the other one and will be removed
# fit2 <- lm(mpg ~ factor(am) + hp + drat + wt  + factor(vs) , data=cars)
# vif(fit2)
# summary(fit2)
# 
# 
# 
# anova(fit0,fit2)
# 
# 
# 
# fit3 <- lm(mpg ~ factor(am) + hp + wt  + factor(vs) , data=cars)
# vif(fit3)
# summary(fit3)
# 
# 
# # fit2 <- lm(mpg ~ factor(am) + cyl + disp + hp + drat + wt  + factor(vs) + factor(gear) , data=cars)
# # vif(fit2)
# # summary(fit2)
# # 
# # fit3 <- lm(mpg ~ factor(am) + cyl + disp + hp + drat + wt  + factor(vs), data=cars)
# # vif(fit3)
# # summary(fit2)
# # 
# # fit4 <- lm(mpg ~ factor(am) +  disp + hp +  wt  + factor(vs) + factor(gear) , data=cars)
# # vif(fit4)
# # summary(fit2)
# 
# 
# 
# 
# 
# # data(SALARY)
# # M <- lm(salary~.,data=salary)
# # VIF(M)
# 
