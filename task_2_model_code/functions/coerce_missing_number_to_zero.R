# a function to coerce missing numbers to zero.
coerce_missing_number_to_zero <- function(x) {
    
    
    # coerce missing values to eithier a  0 double or 0 integer or return an error, dependent on the class of x.
    if(is.double(x)) {
        
        dplyr::if_else(is.na(x), 0, x)
        
        
    } else if(is.integer(x)) {
        
        dplyr::if_else(is.na(x), 0L, x)
        
    } else {
        
        stop('x must be a double or an integer')    
        
    }
    
}