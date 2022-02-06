#the true regression model is a line of mean values (straight line)
#Y = a + bX + e , x: regressor or predictor, Y: dependent or response variable
#a,b = unkown regression coefficients 
#e = epsilon, random error with mean 0 and variance = (sigma^2),uncorrelated
#method of least squares : estimating the parameters (a,b) to minimize 
#the sum of the squares of the vertical deviations

df <- data.frame(Observation = numeric(0),
                       HydrocarbonLevel = numeric(0),
                       purity = numeric(0))

reg_data <- read.csv("reg_examples.csv",header = TRUE)
View(reg_data)

#check normality
library(ggpubr)

hist(reg_data$Hydrocarbon.Level)
hist(reg_data$Purity)

shapiro.test(reg_data$Hydrocarbon.Level)
shapiro.test(reg_data$Purity)

ggqqplot(reg_data$Hydrocarbon.Level,ylab = "Hlevel")
ggqqplot(reg_data$Purity,ylab = "purity")

#linearity
plot(Purity ~ Hydrocarbon.Level , data = reg_data)

#regression analysis
l <- lm(Purity ~ Hydrocarbon.Level , data = reg_data)
summary(l)

#analysis of variance
av <- stats::aov(Purity ~ Hydrocarbon.Level , data = reg_data)
summary(av)

#estimations : a = 14.88 , b = 74.35 , variance = 1.25

#Confidence Interval
confint(l)

#one sample t-test
t.test(reg_data$Hydrocarbon.Level)
t.test(reg_data$Purity)
t.test(reg_data$Purity,conf.level = 0.9)

#confidence interval for the mean of normal data
norm.interval = function(data, variance = var(data), conf.level = 0.95) {
   z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
   xbar = mean(data)
   sdx = sqrt(variance/length(data))
   c(xbar - z * sdx, xbar + z * sdx)
}

norm.interval(reg_data$Hydrocarbon.Level)

#confidence interval for the variance of normal data
var.interval = function(data, conf.level = 0.95) {
   df = length(data) - 1
   chilower = qchisq((1 - conf.level)/2, df)
   chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
   v = var(data)
   c(df * v/chiupper, df * v/chilower)
     }

var.interval(reg_data$Hydrocarbon.Level)
var(reg_data$Hydrocarbon.Level)

###
library(BSDA)

attempts = 100
curve(expr = dnorm(x, mean = 40, sd = 2), from = 33, to = 48,
      main = "95% CI simulation results for sample size = 20",
      xlab = "Chocolate Bar Weight (in g)", ylab = "Density",
      lwd=2, col="blue")
abline(v = 40, col = "purple", lwd = 2)
failed_to_contain <- 0
for (i in 1:attempts) {
  col <- rgb(0,0,0,0.5)
  choc.batch <- rnorm(n = 20, mean = 40, sd = 2) 
  myCI <- z.test(choc.batch, sigma.x = 2, conf.level = 0.95)$conf.int
  if (min(myCI) > 40 | max(myCI) < 40) {
    failed_to_contain <- failed_to_contain + 1
    col <- rgb(1,0,0,1)
  }
  segments(min(myCI), 0.2 * i / attempts,
           max(myCI), 0.2 * i / attempts,
           lwd = 1, col = col)
}

#Boxplot
library(reshape)

re <- melt(as.data.frame(reg_data),
           measure.vars = 1,
           variable_name = "Hydrocarbon"
           #id.vars = 
           )
car::Boxplot(value~Hydrocarbon, 
             data = re,
             col = "lightblue",
             id = list(lables = re$Hydrocarbon,
                       col = "red"))

boxplot(x = reg_data$Hydrocarbon.Level)

#compare observation vs fitted values
plot(Purity ~ Hydrocarbon.Level, 
     data = reg_data,
     xlab = "purity",
     ylab = "Hydrocarbon",
     pch = 1 ,col = "blue")
abline(l,col = "red")
points(l$fitted.values~reg_data$Hydrocarbon.Level,pch = 2 , col = "red")
legend("topleft",
       legend = c("observed","fitted"),
       pch = c(1,2),
       col = c("blue","red"))

#check model assumption
par(mfrow = c(2,2))
plot(l,pch = 16 , col = "blue")

dev.off()

plot(l$residuals,pch = 16,col="blue")
qqline(l$residuals,col = "red")


#prediction
new <- data.frame(x = seq(1,1.57,0.03))
x <- reg_data$Hydrocarbon.Level
y <- reg_data$Purity
pre <- predict(lm(y~x),
               newdata = new,
               interval = "confidence",
               level = 0.95)

pre2 <- predict(lm(y~x),
                newdata = new,
                interval = "prediction",
                level = 0.95) 

data.frame(Hlevel = new$x , lPurity = pre2)

matplot(new$x, pre,
        lty = c(1,2,2,3,3), type = "l", ylab = "confidence y")
matplot(new$x, pre2,
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

#the prediction interval at the point x is always wider than 
#the confidence interval at x

library(DescTools)
plot(y ~ x)
DrawBand(y = pre2[,2:3],x = new[,1],col = "grey80")
DrawBand(y = pre[,2:3],x = new[,1],col = "grey90")
points(Purity ~ Hydrocarbon.Level,data = reg_data,col = "blue")
abline(l,col = "blue")
legend("topleft",
       legend = c("prediction interval","confidence interval"),
       pch = 15,
       col = c("grey80","grey90"))
dev.off()

#Coefficient of Determination(R2)
#is often used to judge the adequacy of a regression model
#We often refer loosely to R2 as the amount of variability in the data 
#explained or accounted for by the regression model.
#even though R2 is large, this does not necessarily imply that 
#the regression model will provide accurate predictions of future observations.
#SSE : the variation not described by the regression line

#R2 = 1 - SSE/SST
#mentioned as Multiple R-squared in model summary


#CORRELATION
#there is a relationship between the correlation coefficient and the slope(b)
#the coefficient of determination R2 is just  
#the square of the correlation coefficient between Y and X
#R : Coefficient of CORRELATION

Cor(x = reg_data$Hydrocarbon.Level,
    y = reg_data$Purity,
    method = c("pearson", "kendall", "spearman"))

Core_test <- 
  cor.test(x = reg_data$Hydrocarbon.Level,
    y = reg_data$Purity,
    method = c("pearson", "kendall", "spearman"))

Core_test$p.value
Core_test$estimate
Core_test$conf.int


#handle missing values by case-wise deletion
Cor(x = reg_data$Hydrocarbon.Level,
    y = reg_data$Purity,
    method = c("pearson", "kendall", "spearman"),
    use = "complete.obs")

ggscatter(data = reg_data,
          x = "Hydrocarbon.Level",
          y = "Purity",
          add = "reg.line",
          conf.int = TRUE,
          cor.coef = TRUE,
          cor.method = "pearson",
          xlab = "HLevel",
          ylab = "purity")

#lack of fit test
ols_pure_error_anova(l) #available only for simple linear regression
#full model : y = a + bx + c(x^2) + epsilon
Nl <- lm(Purity ~ poly(Hydrocarbon.Level,2) , data = reg_data)
anova(Nl,l)
#result : the full model don't offers better fit model


ggplot(data = reg_data , aes(x = Hydrocarbon.Level,y = Purity)) +
  geom_point() +
  stat_smooth(method = "lm",formula = y ~ poly(x,2),size = 1) +
  xlab("Hlevel") +
  ylab("purity")

#Logistic Regression

#consider the situation where the response variable takes on only 
#two possible values, 0 and 1. These could be arbitrary assignments resulting 
#from observing a qualitative response
#there is considerable empirical evidence indicating that 
#the shape of the response function should be nonlinear
#S-shaped (or reverse S-shaped) function
#This function is called the logit response function,

#odds ratio : If the odds ratio is 2 for a particular value of x,it means that 
#a success is twice as likely as a failure at that value of the regressor x
#exp(1)^(a+bx)
#log odds : when x increases by one unit, This means the odds ratio changes by
#exp(1)^b 
#there for,the slope is the change in the log odds that results from a 
#one-unit increase in x


temp <- read.csv("Temperature.csv")

log_l <- glm(O.Ring.Failure ~ Temperature,data = temp,family = "binomial")
summary(log_l)
#for every one unit change in temperature, the log odd of "O.Ring.Failure"
#changes by "-0.17132"

#odds ratio
exp(coef(log_l))

#confidence intervals for the coefficient estimates
confint(log_l)
confint.default(log_l)

exp(cbind(or = coef(log_l) , confint(log_l)))

shapiro.test(log_l$residuals)
#not normal

f_value <- log_l$fitted.values
ggplot(data = temp) +
  geom_point(aes(x = Temperature,y = O.Ring.Failure)) +
  geom_line(aes(x = Temperature,y = f_value)) 

boxplot(Temperature ~ O.Ring.Failure ,
        ylab = "oring",
        xlab = "temp",
        col = "lightblue",
        data = temp)

#prefer to analysis data that has normal distribution,
#for data with highly skewed,we can analyze them in buckets
#fore example age 
mydata$column <- as.factor(ifelse(mydata$column<=30,"20-30",
                           ifelse(mydata$column<=40,"31-40",
                           ifelse(mydata$column<=50,"41-50","50+"))))

xtabs(~O.Ring.Failure + Temperature , data = temp)

#if the glm model for more than 1 variable has result that some of them have
#no significant p-value, then we can drop them by below function
summary(MASS::stepAIC(log_l))


#proportional odds logistic regression
library(carData)
library(MASS)
library(ggplot2)

data("WVS")
head(WVS)

ggplot(WVS, aes(x = poverty, y = age, fill = poverty)) +   
  geom_boxplot(size = .75) +   
  facet_grid(country ~ gender, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

model_fit <- polr(poverty~religion+degree+country+age+gender, 
                  data = WVS, 
                  Hess = TRUE)
summary(model_fit)

#calculate p-value
summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

new_data <- data.frame("religion"= "yes",
                       "degree"="no",
                       "country"="Norway",
                       "age"=30,
                       "gender"="male")

predict(model_fit,new_data,type = "p")

#multiple Regression
Wire <- read.csv("WireBond.csv")

#matrix of scatter plot
pairs(~ PullStrength + WireLength + DieHeight,
      data = Wire,lower.panel = panel.smooth)
#
pairs(~ PullStrength + WireLength + DieHeight,
      data = Wire,pch = 19,lower.panel = NULL)
#
lpanel <- function(x, y){
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
upanel <- function(x,y){
  points(x,y,pch = 19)
}

pairs(~ PullStrength + WireLength + DieHeight,
      data = Wire,lower.panel = lpanel,upper.panel = upanel)

library(psych)

pairs.panels(Wire,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             )

library(tidyverse)

multil <- lm(PullStrength ~ WireLength + DieHeight,data = Wire)
summary(multil)
anova(multil)

#just for study
coefficients(multil)
coef(multil)
confint(multil)
deviance(multil)
effects(multil)
#Variance-covariance matrix of the main parameters
vcov(multil)

sd <- data.frame(WireLength=8,DieHeight=275)
predict(multil,sd,interval = "predict")
predict(multil,sd,interval = "confidence")

#Residual Analysis
mr <- multil$residuals

plot(Wire$WireLength,mr,pch = 19,col = "blue")
abline(a = 0,b = 0,col = "blue")

plot(Wire$DieHeight,mr,pch = 19,col = "darkgreen")
abline(a = 0,b = 0,col = "darkgreen")

#Influential Observations 
#x-space observations

#hat matrix diagonals
dia <- hatvalues(multil)

cookds <- cooks.distance(model = multil,hat = dia)
round(cookds,3)

library(olsrr)
ols_plot_cooksd_bar(multil)
ols_plot_dffits(multil)
ols_plot_dfbetas(multil)
ols_plot_resid_stud(multil)
ols_plot_resid_lev(multil)
ols_plot_resid_stud_fit(multil)
ols_plot_resid_pot(multil)

plm <- lm(PullStrength ~ WireLength + poly(DieHeight,2) ,data = Wire)
anova(multil,plm)

#stepwise selection variables
wine <- read.csv("WineQuality.csv")
head(wine)
summary(wine)

wl <- lm(quality ~ Oakiness + Flavor + Body + Aroma + Clarity,
           data = wine)
summary(wl)

swl <- stepAIC(wl)
summary(stepAIC(wl))

pairs(~ quality + Oakiness + Flavor + Body + Aroma + Clarity,
      data = wine,lower.panel = panel.smooth,pch = 19 , col = "blue")

library(qpcR)
pwl <- PRESS(wl)
pswl <- PRESS(swl)

data.frame(reg = pwl$stat , stepwise_reg = pswl$stat)


#Multicollinearity
#dependencies among the regressor variables,where these dependencies are strong
#we say that multicollinearity exists.
#thus as a result we define the "variance inflation factor"
#results in ordinary least squares estimators that may have extremely large variances

#Some authors have suggested that if any variance inflation factor exceeds 10,
#multicollinearity is a problem. Other authors consider this value too liberal
#and suggest that the variance inflation factors should not exceed 4 or 5

#If the F-test for significance of regression is significant, 
#but tests on the individual regression coefficients are not significant, 
#multicollinearity may be present

ols_vif_tol(wl)
#Tolerance : 1-R^2 is Percent of variance in the predictor that cannot be 
#accounted for by other predictors

#Condition Index
ols_eigen_cindex(wl)

#mean Eigenvalue = 1 , sum Eigenvalue = 6 -- count of variables
#condition index = sqrt(Eigenvalue[1]/Eigenvalue[i])
#condition number = max(condition index)
#condition number between 10 and 30 indicates the presence of multicollinearity
#when a value is larger than 30, the multicollinearity is regarded as strong.
cindx <- ols_eigen_cindex(wl)
max(cindx$`Condition Index`) #condition number

ols_coll_diag(wl)

#VDP : Variance Decomposition Proportion
#The total sum of the VDP for one explanatory variable is 1
#enable the determination of the variables involved in the multicollinearity
#If two or more VDP corresponding to condition indices higher than 10 to 30 
#exceed 80% to 90%, it is determined that multicollinearity is present between 
#the explanatory variables corresponding to the exceeding VDP.


ols_plot_resid_fit_spread(wl)
#It shows how much variation in the data is explained by the fit 
#and how much remains in the residuals

ols_plot_obs_fit(wl)
#If model had a high R Square,all the points would be close to this diagonal line

ols_plot_diagnostics(wl)
#Panel of plots for regression diagnostics


ols_plot_added_variable(wl)

#ridge regression
#is suitable for situations where the multicollinearity problem exists.
#find a set of regression coefficients that are more "stable" in the sense that
#they have a small mean square error

library(ridge)
wlr <- linearRidge(quality ~ Oakiness + Flavor + Body + Aroma + Clarity,
            data = wine,
            lambda =c(0.01,0.064,0.5,0))







