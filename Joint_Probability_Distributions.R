###DISCRETE RANDOM VARIABLES

#In the development of a new receiver for the transmission of digital information
#each bit is rated as acceptable, suspect, or unacceptable
#with probabilities 0.9, 0.08, and 0.02, respectively
# n = 4 , X,Y with binomial Distributions

#Joint Probability Distributions
#P(X = 2 , Y = 1) = 0.0156 

#Marginal Probability Distributions
#P(X = 3) = P(X = 3 , Y = 0) + P(X = 3 , Y = 1) = 0.292  

#Conditional Probability Distributions
#P(Y=0|X=3) = P(X = 3 , Y = 0) / P(X = 3) = 0.2 

#Independence
#In a large shipment of parts, 1% of the parts do not conform to specifications. The supplier
#inspects a random sample of 30 parts, and the random variable X denotes the number of parts
#in the sample that do not conform to specifications. The purchaser inspects another random
#sample of 20 parts
#Although the samples are typically selected without replacement, if the shipment is large,
#relative to the sample sizes being used, approximate probabilities can be computed
#With this assumption,
#the marginal probability distribution of X is binomial
#P(X<=1,Y<=1) = P(X<=1)*(Y<=1) = 0.948



Fn <- function(X,Y,N,Px,Py){
  
  factor <- factorial(N)/(factorial(X)*factorial(Y)*factorial(N-X-Y))
  
  if (X + Y > N) {
    stop("Check your Parameter")
  }
  if (missing(Y)){
    Y <- N-X
  }
  
  return(factor*(Px^X)*(Py^Y)*((1-Px-Py)^(N-X-Y)))
}

Fn_Condition <- function(X,Y,N,Px,Py){
  if (X + Y > N) {
    stop("Check your Parameter")
  }
  
  return(Fn(X = X,Y = Y,N = N,Px = Px,Py = Py)/
           dbinom(x = X,size = N,prob = Px))
}

Fn_Independece <- function(X,Y,Nx,Ny,P){
  if (X + Y > N) {
    stop("Check your Parameter")
  }
  
  binom_x <- pbinom(q = X,size = Nx,prob = P)
  binom_y <- pbinom(q = Y,size = Ny,prob = P)
  return(binom_x*binom_y)
}


Fn(X = 2,Y = 1,N = 4,Px = 0.9,Py = 0.08)
Fn_Condition(X = 3,Y = 0,N = 4,Px = 0.9,Py = 0.08)


N = 4
Joint_Matrix <- matrix(nrow = N+1 , ncol = N+1)

for (X in 0:N) {
  for (Y in 0:N) {
    if(X+Y<=4){
      Joint_Matrix[X+1,Y+1]=Fn(X = X,Y = Y,N = N,Px = 0.9,Py = 0.08)
    }
  }
}
#Joint_Matrix[1:4,1:5]

rownames(Joint_Matrix) <- c(0:N)
colnames(Joint_Matrix) <- c(0:N)
Joint_Matrix[is.na(Joint_Matrix)] <- 0

#Joint Probability Distributions
Joint_Matrix

ycol <- apply(Joint_Matrix,2,sum)
xrow <- apply(Joint_Matrix,1,sum)

P_Y <- as.matrix(round(ycol,5))
colnames(P_Y) <- "Y"
P_X <- as.matrix(xrow) 
colnames(P_X) <- "X"

#Marginal Probability Distributions
P_X
P_Y


N = 4
Condition_Matrix <- matrix(nrow = N+1 , ncol = N+1)

for (X in 0:N) {
  for (Y in 0:N) {
    if(X+Y<=4){
      Condition_Matrix[X+1,Y+1]=Fn_Condition(X = X,Y = Y,N = N,Px = 0.9,Py = 0.08)
    }
  }
}

rownames(Condition_Matrix) <- c(0:N)
colnames(Condition_Matrix) <- c(0:N)
Condition_Matrix[is.na(Condition_Matrix)] <- 0

#Conditional Probability Distributions
Condition_Matrix

#Independence
Fn_Independece(1,1,30,20,0.01)
