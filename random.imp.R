random.imp <- function (a){
    missing <- is.na(a)
    n.missing <- sum(missing)
    a.obs <- a[!missing]
    imputed <- a
    imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
    return (imputed)
}

