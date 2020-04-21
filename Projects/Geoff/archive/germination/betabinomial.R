betabinomial <- function(k, n, shape1, shape2){
    suppressWarnings(
        tmp <- lchoose(n, k) + lbeta(a = k + shape1, n - k + shape2) - (lbeta(shape1, shape2))
    )
    return(exp(tmp))
}
