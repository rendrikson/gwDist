# article: Gromov–Wasserstein Distances and the
#          Metric Approach to Object Matching
# author : Facundo Mémoli
# computational techique (lk 466)

# F = min{U^T G U, over U}
# U_0 - product measure of mu_X and mu_Y
# U_{n+1} = argmin{U^T G U_n, over U w same constraints}

# function for solving successive LOP's
gw_dist <- function(initial_values,d_X,d_Y,mu_X,mu_Y, tol = 0.001, p = 1)
{
      if (!require("pacman")) install.packages("pacman")
      pacman::p_load(Rglpk)
      # change in objective function
      change <- 1
      G <- mat_G(d_X = d_X, d_Y = d_Y)
      U0 <- initial_values
      res <- c()
      obj <- G %*% U0
      mat <- mu_constraints(mu_X,mu_Y)
      dir <- rep("==", nrow(mat))
      rhs <- c(mu_X,mu_Y)

      bounds <- list(lower = list(ind = c(1:ncol(mat)),
                                  val = c(rep(0,ncol(mat)))),
                     upper = list(ind = c(1:ncol(mat)),
                                  val = c(rep(1,ncol(mat)))))

      result <- Rglpk_solve_LP(obj = obj, mat = mat, dir = dir,
                               rhs = rhs, max = FALSE, bounds = bounds)
      res <- c(res,result$optimum)

      # iterative part
      while(tol < change)
      {
            obj <- G %*% result$solution
            mat <- mu_constraints(mu_X,mu_Y)
            dir <- rep("==", nrow(mat))
            rhs <- c(mu_X,mu_Y)

            bounds <- list(lower = list(ind = c(1:ncol(mat)),
                                        val = c(rep(0,ncol(mat)))),
                           upper = list(ind = c(1:ncol(mat)),
                                        val = c(rep(1,ncol(mat)))))

            result <- Rglpk_solve_LP(obj = obj, mat = mat, dir = dir,
                                     rhs = rhs, max = FALSE, bounds = bounds)
            res <- c(res,result$optimum)
            change <- abs(tail(res,2)[1] - tail(res,2)[2])
      }

      distance <- 0.5*H_mu(mu = result$solution, X ,Y,d_X,d_Y,mu_X,mu_Y,p = p)
      return(list("optimum" = distance, "steps" = res))
}
