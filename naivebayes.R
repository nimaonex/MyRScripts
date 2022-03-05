#this document is based on Cran document for naivebayes library

library(naivebayes)
###Simulate the data
cols <- 10
rows <- 100
probs <- c("0" = 0.9,"1" = 0.1)
M <- matrix(sample(0:1,rows*cols,TRUE,probs),nrow = rows,ncol = cols)
Y <- factor(sample(paste0("class",LETTERS[1:2]),rows,TRUE,prob = c(0.3,0.7)))
colnames(M) <- paste0("V",seq_len(ncol(M)))
laplace <- 0
### Train the Bernoulli Naive Bayes
bnb <- bernoulli_naive_bayes(x = M , y = Y , laplace = laplace)
summary(bnb)
# Classification
head(predict(bnb,newdata = M,type = "class")) # head(bnb %class% M)
# Posterior probabilities
head(predict(bnb,newdata = M,type = "prob"))  # head(bnb %prob% M)
# Parameter estimates
coef(bnb)
### Sparse data: train the Bernoulli Naive Bayes
library(Matrix)
M_sparse <- Matrix(M,sparse = TRUE)
class(M_sparse)  # dgCMatrix
# Fit the model with sparse data
bnb_sparse <- bernoulli_naive_bayes(M_sparse,Y,laplace = laplace)
# Classification
head(predict(bnb_sparse, newdata = M_sparse, type = "class"))
# Posterior probabilities
head(predict(bnb_sparse, newdata = M_sparse, type = "prob"))
# Parameter estimates
coef(bnb_sparse)
### Equivalent calculation with general naive_bayes function.
# Make sure that the columns are factors with the 0-1 levels
df <- as.data.frame(lapply(as.data.frame(M),factor,levels=c(0,1)))
nb <- naive_bayes(df,Y,laplace = laplace)
summary(nb)
# Obtain probability tables
tables(nb,which = "V1")
tables(bnb,which = "V1")
# Visualise class conditional Bernoulli distributions
plot(nb,"V1",prob = "conditional")
plot(bnb,"V1",prob = "conditional")
### Train the Gaussian Naive Bayes
gnb <- gaussian_naive_bayes(x= as.matrix(iris[-5]),y=iris[[5]])
### Extract coefficients
coef(gnb)
coef(gnb)[c(TRUE,FALSE)] # only means
coef(gnb)[c(FALSE,TRUE)] # only standard deviations
# Check the equivalence of the class conditional distributions
all(get_cond_dist(nb) == get_cond_dist(gnb))
###some examples
nb<- naive_bayes(Species ~ .,iris)
newdata <- iris[1:5,-5]
#classification
nb %class% newdata #Infix operators 
predict(nb, newdata, type = "class")
# Posterior probabilities
nb %prob% newdata #Infix operators
predict(nb, newdata, type = "prob")
### Train the Multinomial Naive Bayes
mnb <- multinomial_naive_bayes(x=M,y=Y,laplace = laplace)
### naive_bayes function
n <- 100
set.seed(1)
data <- data.frame(class = sample(c("classA","classB"),n,TRUE),
                   bern = sample((LETTERS[1:2]),n,TRUE),
                   cat = sample(letters[1:3], n, TRUE),
                   logical =  sample(c(TRUE,FALSE), n, TRUE),
                   norm = rnorm(n),
                   count = rpois(n,lambda = c(5,15)))
nb2 <- naive_bayes(class ~ . ,data)
summary(nb2)
test <- data[96:100, -1]
nb2%prob%test
#and follow other above examples
###Model continuous variables non-parametrically 
###via kernel density estimation (KDE)
nb_kde <- naive_bayes(class ~ . , data , usekernel = TRUE)
summary(nb_kde)
get_cond_dist(nb_kde)
nb_kde%prob%test
plot(nb_kde,"norm",arg.num = list(legend.cex = 0.9),prob = "conditional")
plot(nb_kde,"count",arg.num = list(legend.cex = 0.9),prob = "conditional")
# Change Gaussian kernel to biweight kernel
nb_kde_biweight <- naive_bayes(class ~ . , data , usekernel = TRUE , kernel = "biweight")
nb_kde_biweight%prob%test
#Adjust bandwidth
naive_bayes(class ~. , data , usekernel = TRUE , adjust = 1.5 )
###Model non-negative integers with Poisson distribution
nb_pois <- naive_bayes(class ~ . , data , usekernel = TRUE , usepoisson = TRUE)
summary(nb_pois)
get_cond_dist(nb_pois)
plot(nb_pois,"count",prob = "marginal")
### Train the Non-Parametric Naive Bayes
ny <- iris[[5]]
nM <- as.matrix(iris[-5])
nnb <- nonparametric_naive_bayes(x = nM , y = ny)
nnb_kernel <- nonparametric_naive_bayes(x = nM, y = ny, kernel = "biweight")
plot(nnb_kernel,1,prob = "conditional")
# Visualize class marginal probabilities corresponding to the first feature
bnb <- bernoulli_naive_bayes(x = M, y = y, laplace = laplace)
plot(bnb, which = 1)
gnb <- gaussian_naive_bayes(x = nM, y = ny, prior = c(0.1,0.3,0.6))
plot(gnb, which = 1)
iris2 <- cbind(iris, New = sample(letters[1:3], 150, TRUE))
nb <- naive_bayes(Species ~ ., data = iris2, prior = c(0.1, 0.3, 0.6))
plot(nb, which = 1, ask = FALSE, prob = "conditional",
     arg.num = list(col = 1:3, lty = 1,
                    main = "Naive Bayes Plot", legend.position = "topright",
                    legend.cex = 0.55))
plot(nb, which = 1, ask = FALSE, prob = "marginal",
     arg.num = list(col = 1:3, lty = 1,
                    main = "Naive Bayes Plot", legend.position = "topright",
                    legend.cex = 0.55))
### Train the Poisson Naive Bayes
### Equivalent calculation with general naive_bayes function.
pnb <- poisson_naive_bayes(x = M, y = Y, laplace = laplace)
nb <- naive_bayes(M, Y, laplace = laplace, usepoisson = TRUE)
