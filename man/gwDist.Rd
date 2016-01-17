\name{gwDist}
\alias{gwDist()}
\docType{package}
\title{
      Function for computing Gromov-Wasserstein distance between metric measure spaces
}
\description{
      Function for computing Gromov-Wasserstein distance between metric measure spaces.
}
\usage{
      gwDist(initial_values,d_X,d_Y,mu_X,mu_Y, tol = 0.001, p = 1)	
}

\arguments{
      initial_values    smth
      d_X               smth
}

\examples{
      \dontrun{
            library(gwDist)
# load metric measure spaces of 3D animal data into worksapce
data(mmspaces_3D)
# subset of metric measure spaces
mm_sub <- mmspaces_3D[c(12:16,61:65)]

# compute GW distance between 10 objects selected
gw_mat <- matrix(nrow = length(mm_sub), ncol = length(mm_sub))
for(i in 1:length(mm_sub))
{
      X <- mm_sub[[i]]$points
      d_X <- mm_sub[[i]]$dist
      mu_X <- mm_sub[[i]]$prob
      for(j in i:length(mm_sub))
      {
            Y <- mm_sub[[j]]$points
            d_Y <- mm_sub[[j]]$dist
            mu_Y <- mm_sub[[j]]$prob
            # compute initial values using solve_FLB_Rglpk
            sol <- solve_FLB_Rglpk(X,Y,d_X,d_Y,mu_X,mu_Y)$solution
            # compute Gromov-Wasserstein distance 
            gw_mat[i,j] <- gwDist(sol, d_X, d_Y, mu_X, mu_Y)$optimum
            gw_mat[j,i] <- gw_mat[i,j]
      }
}

rownames(gw_mat) <- colnames(gw_mat) <- names(mm_sub)
heatmap(gw_mat)
      }
}
