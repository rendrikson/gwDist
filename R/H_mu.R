# article: Gromov–Wasserstein Distances and the
#          Metric Approach to Object Matching
# author : Facundo Mémoli
# computational techique (lk 466)

# Function to minimize

H_mu <- function(mu,X ,Y,d_X,d_Y,mu_X,mu_Y,p = 1)
{
      d_X <- as.matrix(d_X)
      d_Y <- as.matrix(d_Y)
      mu <- matrix(mu, nrow = nrow(d_X), byrow = TRUE)
      return(H_mu_typed(mu = mu, d_X = d_X, d_Y = d_Y))
}
