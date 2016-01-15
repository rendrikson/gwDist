# article: Gromov–Wasserstein Distances and the 
#          Metric Approach to Object Matching
# author : Facundo Mémoli
# implementation of FLB (lk 467)
# constraints to mu (lk 466)
mu_constraints <- function(mu_X,mu_Y)
{
      mu <- matrix(NA, nrow = length(mu_X), 
                   ncol = length(mu_Y), byrow = T)
      mu_pos <- matrix(c(1:length(mu)), byrow = T, 
                       nrow = length(mu_X), ncol = length(mu_Y))
      
      c_mat <- matrix(0,nrow = length(mu_X) + length(mu_Y), 
                      ncol = length(mu))
      for(i in 1:(length(mu_X) + length(mu_Y)))
      {
            if(i <= length(mu_X))
            {
                  c_mat[i,c(mu_pos[i,])] <- 1
            }
            else
            {
                  c_mat[i,c(mu_pos[,i-length(mu_X)])] <- 1    
            }
      }  
      # returns matrix with n_X + n_Y rows (nr of linear constraints) and
      # n_X * n_Y columns (nr of mu_ij's)
      # each row satifies left side of one linear constraint
      # right side = mu_X(1), mu_X(2), ..., mu_Y(1), mu_Y(2), ...
      # each column marks one of mu_ij (mu_11,mu_12, mu_13, ..., mu_21, ... )
      return(c_mat)
}