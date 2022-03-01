library(purrr)

###Uniform Distribution
a <- 1
b <- 6
dice <- seq(a,b,1)
rdunif(6,b,a) #create 6 random int between 6(max) and 1(min)
y = dunif(dice,a,b)
plot(dice, y, type = 'l', ylim = c(0,max(y)+0.1))
cord.a = c(a,seq(1,2,1),2)
cord.b = c(0,dunif(seq(1,2,1),1,6),0)
polygon(cord.a,cord.b,col = "blue")
#p(x<c) cumulative
c <- 3
punif(c,1,6)
1-punif(c,1,6)
qunif(0.4,a,b) #quantile 40%

###BINOMIAL DISTRIBUTION
#dbinom(x, size, prob)
#pbinom(x, size, prob)
#qbinom(p, size, prob)
#rbinom(n, size, prob)
#x is a vector of numbers.
#p is a vector of probabilities.
#n is number of observations.
#size is the number of trials.
#prob is the probability of success of each trial.
df <- data.frame(x = numeric(0),outcome = character(0))
digit <- edit(df)
#the number of bits in error in the next four bits transmitted. 
#the chanse of error is 0.1
dbinom(2 , 4 , 0.1)
r <- dbinom(0:4 , 4 , 0.1)
plot(0:4 , r , type = "l")

pbinom(2 , 4 , 0.1)
pbinom(2 , 4 , 0.1 , lower.tail = FALSE)

qbinom(0.7 , 4 , 0.1)
rbinom(4 , 4 , 0.1)

###Geometric Distribution
#trials are conducted until a success is obtained
dgeom(x = 5 , 0.1 , log = FALSE )
y <- dgeom(0:20 , 0.1 , log = FALSE )
plot(0:20,y , type = "l")

pgeom(5,0.1,lower.tail = TRUE,log.p = FALSE)
sum(dgeom(0:5,0.1,log = FALSE))

qgeom(0.5,0.1,lower.tail = TRUE,log.p = FALSE)
plot(qgeom(seq(1,20,by=1),prob = 0.1))
plot(qgeom(seq(0,1,by=0.01),prob = 0.5))

rgeom(5,0.1)
table(factor(rgeom(5,0.1),0:max(rgeom(5,0.1))))
  
library(dplyr)
library(ggplot2)

p = 0.20
n = 3

data.frame(x = 1:20, 
           pmf = dgeom(x = 0:19, prob = p),
           cdf = pgeom(q = 0:19, prob = p, lower.tail = TRUE)) %>%
  ggplot(aes(x = factor(x), y = cdf)) +
  geom_col() +
  geom_text(
    aes(label = round(cdf,2), y = cdf + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of X = x trials to achieve first success",
       subtitle = "Geometric(.2)",
       x = "Trials (x)",
       y = "probability")

data.frame(x = 0:10, prob = dgeom(x = 0:10, prob = p)) %>%
  mutate(Failures = ifelse(x == n, n, "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,2), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of X = 3 Failures Prior to First Success",
       subtitle = "Geometric(.2)",
       x = "Failures prior to first success (x)",
       y = "Probability")

###Negative Binomial Distribution
#A generalization of a geometric distribution in which 
#the random variable is the number of
#Bernoulli trials required to obtain r successes
dnbinom(x = 6 , size = 4 , prob = 0.1 , log = FALSE) #4th trial in 10th observation

library(dplyr)
library(ggplot2)

n <- 6
data.frame(x = 0:6,prob = dnbinom(x = 0:6,size = 4,prob = 0.1,log = FALSE)) %>%
  mutate(Failures = ifelse(x == n, n, "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,6), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r = 3 Successes in X = 7 Trials",
       subtitle = "NB(3,.2)",
       x = "Failed Trials (X - r)",
       y = "Probability") 

y <- pnbinom(q = 0:6, size = 4 , prob = 0.1 , lower.tail = TRUE, log.p = FALSE)
plot(x = 0:6 , y = y , type = "l")

s <- seq(0,1,0.1)
qy <- qnbinom(0.25, size = 4 , prob = 0.1 , lower.tail = TRUE, log.p = FALSE)
qys <- qnbinom(s , size = 4 , prob = 0.1 , lower.tail = TRUE, log.p = FALSE)
plot(s , qys , type = "l")

rnbinom(n = 10 , size = 4 , prob = 0.1 )

library(graphics)

x1 <- rnbinom(500, mu = 4, size = 1)
x2 <- rnbinom(500, mu = 4, size = 10)
x3 <- rnbinom(500, mu = 4, size = 100)
h1 <- hist(x1, breaks = 20, plot = FALSE)
h2 <- hist(x2, breaks = h1$breaks, plot = FALSE)
h3 <- hist(x3, breaks = h1$breaks, plot = FALSE)
barplot(rbind(h1$counts, h2$counts, h3$counts),
        beside = TRUE, col = c("red","blue","cyan"),
        names.arg = round(h1$breaks[-length(h1$breaks)]))

###HYPERGEOMETRIC DISTRIBUTION
#n parts are selected at random, without replacement
#N is Total Number, a is Problem NUmber 
#for example 50 of 850 daily product is damaged
#the trials are not independent
dhyper(x = 0:2 , m = 50 , n = 800 , k = 2)
round(dhyper(x = 0:2 , m = 50 , n = 800 , k = 2),3)
data.frame(x = 0:2 , prob = round(dhyper(x = 0:2 , m = 50 , n = 800 , k = 2),3))

#check out
m <- 10; n <- 7; k <- 8
x <- 0:(k+1)
rbind(phyper(x, m, n, k), dhyper(x, m, n, k))
all(phyper(x, m, n, k) == cumsum(dhyper(x, m, n, k)))  # FALSE
# but error is very small:
signif(phyper(x, m, n, k) - cumsum(dhyper(x, m, n, k)), digits = 3)


library(tidyr)
library(ggplot2)
library(dplyr)
options(scipen = 999, digits = 2) # sig digits

x = 14
m = 7000
n = 3000
k = 20
d_binom <- dbinom(x = 1:20, size = k, prob = m / (m + n))
df_binom <- data.frame(x = 1:20, Binomial = d_binom)
p <- ggplot(df_binom, aes(x = x, y = Binomial)) +
  geom_col()

d_hyper_100 <- dhyper(x = 1:20, m = 70, n = 30, k = k)
d_hyper_250 <- dhyper(x = 1:20, m = 175, n = 75, k = k)
d_hyper_500 <- dhyper(x = 1:20, m = 350, n = 150, k = k)
d_hyper_1000 <- dhyper(x = 1:20, m = 700, n = 300, k = k)
df_hyper = data.frame(x = 1:20, 
                      Hyper_100 = d_hyper_100, 
                      Hyper_250 = d_hyper_250, 
                      Hyper_500 = d_hyper_500, 
                      Hyper_1000 = d_hyper_1000)
df_hyper_tidy <- gather(df_hyper, key = "dist", value = "density", -c(x))
p + 
  geom_line(data = df_hyper_tidy, aes(x = x, y = density, color = dist)) +
  labs(title = "Hypergeometric Distribution Appoximation to Binomial",
       subtitle = "Hypergeometric approaches Binomial as population size increases.",
       x = "Number of successful observations (x)",
       y = "Density")

#Recall that the binomial distribution is a satisfactory approximation 
#to the hypergeometric distribution when n, the sample size, 
#is small relative to N, the size of the population from which the sample is 
#selected. A rule of thumb is that the binomial approximation is effective
#if n/N < 0.1

###POISSON DISTRIBUTION
#Given an interval of real numbers, 
#assume counts occur at random throughout the interval. 
#If the interval can be partitioned into subintervals of small enough length 
#the probability of more than one count in a subinterval is zero
#the probability of one count in a subinterval is the same for all subintervals
#and proportional to the length of the subinterval
#the count in each subinterval is independent of other subintervals

dpois(x = 2, lambda = 2.3, log = FALSE) #2 flaws in 1 interval
dpois(x = 2, lambda = 4.6, log = FALSE) #2 flaws in 2 interval

ppois(0:20 , lambda = 4.6 , lower.tail = TRUE, log.p = FALSE)

a <- seq(0,1,0.1)
b <- qpois(a , lambda = 4.6 , lower.tail = TRUE, log.p = FALSE)
plot(x = a , y = b , type = "l")

rpois(n = 10 , lambda = 4.6)

library(ggplot2)
library(dplyr)
options(scipen = 999, digits = 2) # sig digits

events <- 0:10
density <- dpois(x = events, lambda = 3)
prob <- ppois(q = events, lambda = 3, lower.tail = TRUE)
df <- data.frame(events, density, prob)
ggplot(df, aes(x = factor(events), y = density)) +
  geom_col() +
  geom_text(
    aes(label = round(density,2), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "PMF and CDF of Poisson Distribution",
       subtitle = "P(3).",
       x = "Events (x)",
       y = "Density") +
  geom_line(data = df, aes(x = events, y = prob))

#The Poisson distribution approximates the binomial distribution 
#n>=20 , p<=0.05 

library(tidyr)
options(scipen = 999, digits = 2) # sig digits

n = 0:10
df <- data.frame(events = 0:10, 
                 Poisson = dpois(x = n, lambda = .03 * 50),
                 Binomial = dbinom(x = n, size = 50, p = .03))
df_tidy <- gather(df, key = "Distribution", value = "density", -c(events))
ggplot(df_tidy, aes(x = factor(events), y = density, fill = Distribution)) +
  geom_col(position = "dodge") +
  labs(title = "Poisson(15) and Binomial(50, .03)",
       subtitle = "Poisson approximates binomial when n >= 20 and p <= .05.",
       x = "Events (x)",
       y = "Density")

ggplot(df, aes(x = events, y = Poisson , color = "red")) +
  geom_line() +
  geom_line(data = df , aes(x = events , y = Binomial , color = "blue"))