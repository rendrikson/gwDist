# article: Gromov–Wasserstein Distances and the
#          Metric Approach to Object Matching
# author : Facundo Mémoli
# implementation of FLB (lk 467)
# L_p(mu) part
L_p <- function(X,Y,d_X,d_Y,mu_X,mu_Y, p = 1)
{

      sXY <- s_XY(X,Y,d_X,d_Y,mu_X,mu_Y, p = p)
      s_X <- sXY$s_X
      s_Y <- sXY$s_Y

      S <- matrix(NA,nrow = nrow(X), ncol = nrow(Y))
      for(i in 1:nrow(X))
      {
            for(j in 1:nrow(Y))
            {
                  S[i,j] <- abs(s_X[i] - s_Y[j])
            }
      }
      S <- t(S)
      return(list("obj_coef" = c(0.5 * S)))
}
