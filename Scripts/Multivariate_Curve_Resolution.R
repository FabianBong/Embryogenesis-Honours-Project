## load Pracma for inverse matrix
library(pracma)

## Multivariate Curve Resolution
## Make sure that samples have been loaded with BedGraph_Import


## We are solving X = CP - we don't know C or P.
X <- as.matrix(samples[,3:11])
storage.mode(X) <- "numeric"

## Normalize beta value
minVal <- min(X)
maxVal <- max(X)

apply(X, 2, normalizeMinMax,min=minVal,max=maxVal)

## number of CpGs we are looking at
m <- length(X[,1])

## number of samples
n <- length(X[1,])

## we can play around with the basis factors (p)
p <- 6

## Get and estimate of P (m * p)
P_hat <- matrix(runif(p*n,0,2), nrow=p, ncol=n)

## Calculate first C
C_hat <- X%*%t(P_hat)%*%(pinv(P_hat%*%t(P_hat)))

## Check for neg values
C_hat[C_hat < 0] <- 0

old_P_hat = matrix(runif(p*n,0,2),ncol=n, nrow=p )
new_P_hat = P_hat


## While there is a big difference between old P and new P
while(sum(abs(old_P_hat-new_P_hat)) > 0.1)
{
  print(sum(abs(old_P_hat-new_P_hat)))
  old_P_hat <- new_P_hat
  new_P_hat <- pinv(t(C_hat)%*%C_hat, )%*%t(C_hat)%*%X
  
  ## Normalize new_P_hat
  for(i in 1:nrow(new_P_hat))
  {
    new_P_hat[i,] <- scalar1(new_P_hat[i,])
  }
  
  
  C_hat <- X%*%t(new_P_hat)%*%(pinv(new_P_hat%*%t(new_P_hat)))
  C_hat[C_hat < 0] <- 0
  new_P_hat[new_P_hat < 0] <- 0
}

##new_P_hat is now our final P

## plot the new_P_hat rows
par(mfrow = c(1, 1)) 
plot(c(1,1:8),new_P_hat[1,], type ="l")


## Now we wanna find the CpGs that follow the modulation
## Let's use a deviation factor of 0.1
dev <- 0.1

##Vector of probes that follow modulation of one row of P
follow <- vector()

##Which row of P to compare to 
rowP <- 1

## Check each row in X
for(i in 1:nrow(X))
{
  for(j in 1:ncol(X))
  {
    if(j == 9 && X[i,j] > new_P_hat[rowP,j]-dev && X[i,j] < new_P_hat[rowP,j]+dev)
      follow <- c(follow,i)
    if(X[i,j] < new_P_hat[rowP,j]-dev || X[i,j] > new_P_hat[rowP,j]+dev)
      break;
  }
}



## Normalize the given vector
scalar1 <- function(x) {x / sqrt(sum(x^2))}
