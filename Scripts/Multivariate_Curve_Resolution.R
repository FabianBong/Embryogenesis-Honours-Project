## load Pracma for inverse matrix
library(pracma)

##Plotting library
library(ggplot2)
library(gridExtra)
library(ggpubr)

## Multivariate Curve Resolution
## Make sure that samples have been loaded with BedGraph_Import


## We are solving X = CP - we don't know C or P.
X <- as.matrix(samples[,3:11])
storage.mode(X) <- "numeric"

## number of CpGs we are looking at
m <- length(X[,1])

## number of samples
n <- length(X[1,])

## we can play around with the basis factors (p)
p <- 12

## Get and estimate of P (m * p)
P_hat <- matrix(runif(p*n,0,2), nrow=p, ncol=n)

## Calculate first C
C_hat <- X%*%t(P_hat)%*%(pinv(P_hat%*%t(P_hat)))

## Check for neg values
C_hat[C_hat < 0] <- 0

old_P_hat = matrix(runif(p*n,0,2),ncol=n, nrow=p )
new_P_hat = P_hat


## While there is a big difference between old P and new P
while(sum(abs(old_P_hat-new_P_hat)) > 0.01)
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
plotVect <- list()


for(i in 1:nrow(new_P_hat))
{
  plot_df_P <- data.frame(x = c(1,1:8), y = new_P_hat[i,])
  p1 <- ggplot(data = plot_df_P, aes(x,y)) + 
    geom_point(data = plot_df_P, aes(x,y,colour = factor(rownames(plot_df_P)))) +
    geom_line(data = plot_df_P[2:3,], aes(x=x, y = y))+
    geom_line(data = plot_df_P[c(1,3),], aes(x=x, y = y))+
    geom_line(data = plot_df_P[3:9,], aes(x=x, y = y)) + 
    ylab(i) + 
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_text(angle=0, face = "bold",vjust = 0.5),
          legend.title = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  plotVect[[i]] <- p1
}

ggarrange(plotlist = plotVect, ncol = 1, nrow = p, common.legend =  TRUE)

do.call("grid.arrange", c(plotVect, ncol = 1))

dev.off()


## Now we wanna find the CpGs that follow the modulation
## Let's use a deviation factor of 0.1
dev <- 0.1

##Vector of probes that follow modulation of one row of P
follow <- vector()

##Which row of P to compare to 
rowP <- 9

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

## Cluster probes as long as they are at most 20 away from each other
clusters = list()
dist = 20
curr_probe = follow[1]
curr_cluster = vector()
curr_cluster <- append(curr_cluster, curr_probe)
for(i in 2:length(follow))
{
  if(follow[i] <= curr_probe + dist)
  {
    curr_cluster <- append(curr_cluster, follow[i])
  }
  else
  {
    clusters <- append(clusters, list(curr_cluster))
    curr_cluster = vector()
    curr_cluster <- append(curr_cluster, follow[i])
  }
  curr_probe <- follow[i]
}
clusters <- c(clusters, list(curr_cluster))

## Only keep clusters of 3 or more probes
keep <- vector()
for(i in 1:length(clusters))
{
  if(length(clusters[[i]]) >= 3)
    keep <- c(keep,i)
}

## All clusters of interest
clusters <- clusters[keep]


## Normalize the given vector
scalar1 <- function(x) {x / sqrt(sum(x^2))}
