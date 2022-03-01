###Continuous Distributions
###probability density function
###Continuous Random variables. each point has probability of zero.

###CONTINUOUS UNIFORM DISTRIBUTION
unif_area <- function(min = 0, max = 1, lb, ub, col = 1,
                      acolor = "lightgray", ...) {
  x <- seq(min - 0.25 * min, max + 0.25 * max, 0.001) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  if(max < min) {
    stop("'min' must be lower than 'max'")
  }
  
  x2 <- seq(lb, ub, length = 1000) 
  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.25 * max, max + 0.25 * max), type = "l",
       ylab = "f(x)", lty = 0, ...)   
  
  y <- dunif(x2, min = min, max = max)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor, lty = 0)
  segments(min, 1/(max - min), max, 1/(max - min), lwd = 2, col = col)
  segments(min - 2 * max, 0, min, 0, lwd = 2, col = col)
  segments(max, 0, max + 2 * max, 0, lwd = 2, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = 2)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = 2)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}

unif_area(min = 0, max = 60, lb = 0, ub = 15)
text(8, 0.008, "25%", srt = 90, cex = 1.2)

punif(30, min = 0, max = 60) - punif(20, min = 0, max = 60)
unif_area(min = 0, max = 60, lb = 20, ub = 30)
text(24, 0.008, "16.7%", srt = 90, cex = 1.2)

x <- seq(-0.5, 1.5, 0.01)
plot(x,punif(x),type="l",lwd=2,col="red",main="uniform CDF",ylab="f(x)")

plot(qunif, punif(0), punif(1), lwd = 2,
     main = "Uniform quantile function",
     xlab = "p", ylab = "Q(p)")
segments(0, 0.5, punif(0.5), 0.5, lty = 2, lwd = 2)
segments(punif(0.5), 0, punif(0.5), qunif(0.5), lty = 2, lwd = 2)

set.seed(1)
runif(n = 10, min = -1, max = 1)

#10 repeat of median of 2 uniform sample
set.seed(1234)  
n_rep <- 10    # Number of repetitions
n <- 5         # Number of points
Median <- numeric(n_rep) 
for (i in 1:n_rep) {
  Median[i] <- median(runif(n))
}
data.frame(Median)

###NORMAL DISTRIBUTION or Gaussian distribution
dnorm(x = 13 , mean = 10 , sd = 2)
dnorm(0:15 , mean = 10 , sd = 2)
plot(x = 0:15 , y = dnorm(0:15 , mean = 10 , sd = 2) , type = "l")

pnorm(q = 13 , mean = 10 , sd = 2)

qnorm(p = 0.25 , mean = 10 , sd = 2)
plot(seq(0,1,0.1) , qnorm(seq(0,1,0.1),mean = 10 , sd = 2) , type = 'l')

rnorm(10)
rnorm(10,mean = 10,sd = 2)

sn <- rnorm(50 , mean = 10 , sd = 2)
hist(sn , right = FALSE)
#dev.off() reset par to default
hist(sn, right = FALSE, breaks = seq(0,20,2), 
     col = "firebrick", las = 1, xlab = "Age at death (yrs)", 
     ylab = "Frequency", main = "")

###NORMAL APPROXIMATION TO THE BINOMIAL AND POISSON DISTRIBUTIONS
pbinom(150,size = 16000000,prob = 10^-5)
pnorm(150,mean = 160,sd = 12.64)

#if E(X) = lambda and V(X) = lambda and lambda > 5
#is approximately a standard normal random variable
ppois(q = 950 , lambda = 1000 , lower.tail =  TRUE , log.p = FALSE)
pnorm(q = 950 , mean = 1000 , sd = sqrt(1000))

###EXPONENTIAL DISTRIBUTION
#The random variable X that equals the distance between successive counts of a
#Poisson process with mean lambda > 0
#mean = 1/lambda , variance = 1/lambda^2

#An exponential random variable describes the length until 
#the first count is obtained in a Poisson process

#large corporate computer network
#Poison process with a mean of 25 log-ons per hour
#Probability that X exceeds 6 minutes, 6/60 = 0.1 hour
#or there are no log-on in a 6 minutes interval
dpois(x = 0 , lambda = 2.5)
ppois(q = 0.1 , lambda = 2.5) #don't like it
#if someone logs on at 2:22 P.M., the probability the next log-on
#occurs after 2:28 P.M. is still 0.082
#the lack of memory property
#P(X < t1 + t2 | X > t1) = p(X < t2)
a <- dexp(x = seq(0,8,0.1), rate = 2, log = FALSE)
b <- dexp(x = seq(0,8,0.1), rate = 1, log = FALSE)
plot(seq(0,8,0.1),a,type = "l",lwd = 2 , col = "red")
lines(seq(0,8,0.1),b,lwd = 2 ,col = "blue")

pexp(0.5 , rate = 1.4)

exp_area <- function(rate = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(0, 12/rate, 0.01) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dexp(x, rate = rate), type = "n", ylab = "")
  
  y <- dexp(x2, rate = rate)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dexp(x, rate = rate), type = "l", ...)
}

exp_area(rate = 0.2, ub = 3, acolor = rgb(0, 0, 1, alpha = 0.5))
text(1, 0.075, "45.12%", srt = 90, col = "white", cex = 1.2)

#Customers arrive at a rate of 0.5 persons per minutes, 
#lambda=0.50, What is the probability of a customer arriving within an 
#interval of x=0 to 3 minutes
pexp(q = 3, rate = 0.5, lower.tail = TRUE)

library(dplyr)
library(ggplot2)

data.frame(x = 0:1000 / 100, prob = pexp(q = 0:1000 / 100, rate = 0.5, lower.tail = TRUE)) %>%
  mutate(Interval = ifelse(x >= 0 & x <= 3, "0 to 3", "other")) %>%
  ggplot(aes(x = x, y = prob, fill = Interval)) +
  geom_area(alpha = 0.3) +
  labs(title = "Cumulative Probability of Interval X until First Success",
       subtitle = "X ~ Exponential(2)",
       x = "Interval (x)",
       y = "Cum Probability") 

#Let X denote the time between detections of a particle with a geiger counter 
#and assume that X has an exponential distribution with lambda = 1.4 minutes. 
#The probability that we detect a particle within 30 seconds of 
#starting the counter is 0.3
1 - round(dexp(x = 0.5 , rate = 1.4),2) # 1 - (exp(1)^(-0.5/1.4))

###Erlang Distribution
#A generalization of the exponential distribution is the length until r counts
#occur in a Poisson process
#mean = r/lambda , variance = r/lambda^2

#failure prob = 0.0001 per hour , 4th error exeed 40000 hour?
ppois(q = 3 , lambda = 4)

###Gamma Distribution
#f(x) = (lambda^r)*(x^(r-1)*())/()
#The Erlang distribution is a special case of the gamma distribution
#If the parameter r of an Erlang random variable is not an integer, but , r > 0
#the chi-squared distribution is a special case of the gamma distribution 
#in which lambda = 1/2 

#someone sends a money order once per 15 minutes, lambda = 0.25 hour
#What is the probability someone sends 10 money orders in less than 3 hours
dgamma(x = seq(0,3,1), shape = 10, scale = 0.25)
pgamma(q = 3, shape = 10, scale = 0.25)

rg <- rgamma(n = 10000, shape = 10 , scale = 0.25)
mean(rg < 3)

library(dplyr)
library(ggplot2)

data.frame(x = 0:1000 / 100, 
           prob = pgamma(q = 0:1000 / 100, 
                         shape = 10, 
                         scale = 0.25, 
                         lower.tail = TRUE)) %>%
  mutate(Interval = ifelse(x >= 0 & x <= 3, "0 to 3", "other")) %>%
  ggplot(aes(x = x, y = prob, fill = Interval)) +
  geom_area(alpha = 0.3) +
  labs(title = "X ~ Gam(alpha = 10, theta = .25)",
       subtitle = "Probability of 10 events in X hours when the mean time to an event is .25 hours.",
       x = "Interval (x)",
       y = "Cum Probability") 

data.frame(x = 0:1000 / 100, 
           prob = dgamma(x = 0:1000 / 100, 
                         shape = 10, 
                         scale = 0.25)) %>%
  mutate(Interval = ifelse(x >= 0 & x <= 3, "0 to 3", "other")) %>%
  ggplot(aes(x = x, y = prob, fill = Interval)) +
  geom_area(alpha = 0.3) +
  labs(title = "X ~ Gam(alpha = 10, theta = .25)",
       subtitle = "Probability of 10 events in X hours when the mean time to an event is .25 hours.",
       x = "Interval (x)",
       y = "Cum Probability") 

###WEIBULL DISTRIBUTION
#As mentioned previously, the Weibull distribution is often used 
#to model the time until failure of many different physical systems
#sigma = Scale , beta = shape 
#when beta = 1, the Weibull distribution is identical to the exponential distribution


#The time to failure (in hours) of a bearing in a mechanical shaft 
#is satisfactorily modeled as a Weibull random variable 
#with sigma = 5000 , beta = 1/2 hour Determine the mean time until failure
#E(X) = 5000*2! = 10000
#Determine the probability that a bearing lasts at least 6000 hours
1 - pweibull(q = 6000 , shape = 0.5 , scale = 5000)
1 - sum(dweibull(x = 1:6000 , shape = 0.5 , scale = 5000,log = FALSE))

library(graphics)
curve(dweibull(x  , 
               shape = 0.5 , 
               scale = 5000),
      from = 1, to = 20, lwd = 2, col = "red")
curve(dweibull(x  , 
               shape = 0.5 , 
               scale = 3000),
      from = 1, to = 20, col = "blue", add = TRUE)
curve(dweibull(x  , 
               shape = 0.5 , 
               scale = 6000),
      from = 1, to = 20, col = "green", add = TRUE)

###LOGNORMAL DISTRIBUTION
# X = exp(w) when w has a normal distribution. That is, the natural logarithm 
#of X is normally distribution.
#mean = theta , variance = omega^2

#The lifetime of a semiconductor laser has a lognormal distribution with
#theta = 10 , omega = 1.5
#What is the probability the lifetime exceeds 10,000 hours

1 - plnorm(q = 10000 , meanlog = 10 , sdlog = 1.5)

rlnorm(10,meanlog = 10,sdlog = 1.5)