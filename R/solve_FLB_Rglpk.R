# solving FLB with package Rglpk
solve_FLB_Rglpk <- function(X, Y, d_X, d_Y, mu_X, mu_Y, p = 1)
{
      if (!require("pacman")) install.packages("pacman")
      pacman::p_load(Rglpk)

      obj0 <- L_p(X,Y,d_X,d_Y,mu_X,mu_Y,p = p)$obj_coef
      mat0 <- mu_constraints(mu_X,mu_Y)
      dir0 <- rep("==", nrow(mat0))
      rhs0 <- c(mu_X,mu_Y)

      bounds0 <- list(lower = list(ind = c(1:ncol(mat0)),
                                  val = c(rep(0,ncol(mat0)))),
                     upper = list(ind = c(1:ncol(mat0)),
                                  val = c(rep(1,ncol(mat0)))))

      result <- Rglpk_solve_LP(obj = obj0, mat = mat0, dir = dir0,
                               rhs = rhs0, max = FALSE, bounds = bounds0)
      # status 0 ==> optimal solution found
      return(result)
}
