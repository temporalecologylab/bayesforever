nll <- function(param, data, pmf, pos, lb, ub){
    pos.regex <- paste(pos, collapse = "|") # collapse "pos" to regex
    pos.tr <- grep(pos.regex, names(param)) # parameters to transform
    param.tr <- replace(param, pos.tr, exp(unlist(param[pos.tr]))) # transform (exp)
    likestor <- dim(length(data))
    for(i in 1:length(data)){
        likestor[i] <- log(do.call(pmf, append(list(data = data[i], lb = lb, ub = ub), param.tr)))
    }
    likestor[is.infinite(likestor)] <- -10000 # Punish 0 likelihoods
    return(-(sum(likestor)))
}
