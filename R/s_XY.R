# article: Gromov–Wasserstein Distances and the 
#          Metric Approach to Object Matching
# author : Facundo Mémoli
# implementation of FLB (lk 467)
# s_X and s_Y part on FLB
s_XY <- function(X,Y,d_X,d_Y,mu_X,mu_Y, p = 1)
{
      d_X <- as.matrix(d_X)
      s_X <- rep(NA,nrow(X))
      for(i in 1:nrow(X))
      {
            s_X[i] <- (mu_X %*% (d_X[,i])^p)^(1/p)
      }
      
      d_Y <- as.matrix(d_Y)
      s_Y <- rep(NA,nrow(Y))
      for(i in 1:nrow(Y))
      {
            s_Y[i] <- (mu_Y %*% (d_Y[,i])^p)^(1/p)
      }
      return(list("s_X" = s_X, "s_Y" = s_Y))
}