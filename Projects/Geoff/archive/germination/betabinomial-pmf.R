betabinomial.pmf <- function(data, lb, ub, shape1, shape2){
    prob <- betabinomial(data - lb, ub - lb, shape1, shape2)
    prob[is.na(prob)] <- 0
    return(prob)
}
