##
## S3 method to 'aov' object
##

TukeyC.aov <-
  function(x,
           which=NULL,
           sig.level=.05,
           round=2, ...)   
  {
    if(is.null(which))
      which <- names(x$model)[2]
    mt <- model.tables(x, "means")               # summary tables for model fits
    if(is.null(mt$n))
      stop("No factors in the fitted model!")
    tabs  <- mt$tables[-1][which]                # specified group means                             
    r     <- mt$n[names(tabs)][[which]]          # groups and its number of replicates
    bal   <- ifelse(length(r) == 1, TRUE, FALSE) # is (or not) balanced
    MSE   <- sum(resid(x)^2) / x$df.residual
    nms   <- names(tabs[[which]])
    ord   <- order(as.vector(tabs[[which]]), decreasing=TRUE)
    lv    <- nlevels(x$model[, which])
    m.inf <- matrix(nrow=lv, ncol=3)
    for(i in 1:lv) { 
      v <- x$model[, 1][x$model[, which] == levels(x$model[, which])[i]]
      m.inf[i, 1] <- mean(v)
      m.inf[i, 2] <- min(v)
      m.inf[i, 3] <- max(v)} 
    m.inf <- cbind(m.inf[, 1][ord], m.inf[, 2][ord], m.inf[, 3][ord])   
    dimnames(m.inf) <- list(nms[ord], c('mean', 'min', 'max'))
    dfr  <- x$df.residual                       # residual degrees of freedom
    out  <- make.TukeyC.test(r=r, MSE=MSE, m.inf=m.inf, ord=ord, sig.level=sig.level,
                             dfr=dfr, bal=bal, mt=mt, round)
    class(out) <- c('TukeyC', 'list')
    return(out) 
  }
