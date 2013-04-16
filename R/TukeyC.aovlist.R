##
## S3 method to 'aovlist' object
##

TukeyC.aovlist <- function(x,
                           which,
                           error,
                           sig.level=.05,
                           round=2,
                           dispersion=c('mm', 's', 'se'), ...)
{
  mt <- model.tables(x,
                     "means")              # summary tables for model fits
  if(is.null(mt$n))
    stop("No factors in the fitted model!")

  tabs <- mt$tables[-1][which]             # specified group means

  r    <- mt$n[names(tabs)][[which]]       # groups and its number of replicates

  bal  <- ifelse(length(r) == 1,
                 TRUE,
                 FALSE)                    # is (or not) balanced

  MSE  <- sum(resid(x[[error]])^2) / x[[error]][[8]]

  nms  <- names(tabs[[which]])

  ord  <- order(as.vector(tabs[[which]]),
                decreasing=TRUE)

  #     ta    <- model.frame.aovlist(x)
  #     lev   <- nlevels(ta[, which])
  #     m.inf <- matrix(nrow=lev, ncol=3)
  #     for(i in 1:lev){ 
  #       v <- ta[, 1][ta[, which] == levels(ta[, which])[i]]
  #       m.inf[i, 1] <- mean(v)
  #       m.inf[i, 2] <- min(v)
  #       m.inf[i, 3] <- max(v)} 
  #     m.inf  <- cbind(m.inf[, 1][ord], m.inf[, 2][ord], m.inf[, 3][ord])
  #     dimnames(m.inf) <- list(nms[ord], c('mean', 'min', 'max'))

#  switch(match.arg(dispersion),
#         mm = {
#           m.inf <- aggregate(model.frame.aovlist(x)[,1],
#                              by=list(model.frame.aovlist(x)[[which]]),
#                              function(x) c(mean=mean(x),
#                                            m.min=min(x),
#                                            m.max=max(x)))[,2]
#         }, se = {
#           m.inf <- aggregate(model.frame.aovlist(x)[,1],
#                              by=list(model.frame.aovlist(x)[[which]]),
#                              function(x) c(mean=mean(x),
#                                            se.min=mean(x) - sd(x),
#                                            se.max=mean(x) + sd(x)))[,2]
#         }, sem= {
#           m.inf <- aggregate(model.frame.aovlist(x)[,1],
#                              by=list(model.frame.aovlist(x)[[which]]),
#                              function(x) c(mean=mean(x),
#                                            sem.min=mean(x) - (sd(x) / sqrt(length(x))),
#                                            sem.max=mean(x) + (sd(x) / sqrt(length(x)))))[,2]
#         })

  m.inf <- m.inf.1b(x,
                    which,
                    dispersion)
  
  rownames(m.inf) <- nms

  m.inf <- m.inf[order(m.inf[,1],
                       decreasing=TRUE),]

  dfr <- x[[error]][[8]]  # residual degrees of freedom

  out <- make.TukeyC.test(r=r,
                          MSE=MSE,
                          m.inf=m.inf,
                          ord=ord,
                          sig.level=sig.level,
                          dfr=dfr,
                          bal=bal,
                          mt=mt,
                          round)

  class(out) <- c('TukeyC',
                  'list')

  return(out)                    
}

