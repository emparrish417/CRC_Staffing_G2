rlst <- function(n, mu = 0, sigma = 1, nu = 10) {
    
    exp(LaplacesDemon::rst(n = n, mu = mu, sigma = sigma, nu = nu))
    
}

dlst <- function(x, mu = 0, sigma = 1, nu = 10, log = FALSE) {
    
    LaplacesDemon::dst(x = log(x), mu = mu, sigma = sigma, nu = nu, log = log)
    
}

