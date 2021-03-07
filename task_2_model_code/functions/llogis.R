rllogis <- function(n, location = 0, scale = 1) {
    
    exp(rlogis(n = n, location = location, scale = scale))
    
}

dllogis <- function(x, location = 0, scale = 1, log = FALSE) {
    
    dlogis(x = log(x), location = location, scale = scale, log = log)
    
}