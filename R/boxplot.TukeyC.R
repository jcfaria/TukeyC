boxplot.TukeyC <- function(x,
                           type.mean = c('line','point','none'),
                           xlab = NULL,
                           col.mean = 'gray',
                           pch.mean = 1,
                           lwd.mean = 1,
                           lty.mean = 1,
                           args.legend = NULL,
                           ...){
  # x is a object of Tukeyc class
  fun <- function(m) {
    a <- rep('\n', length(m))
    a[which(m != '')[1]] <- ''
    return(paste(a, m, sep=''))
  }

  if(!inherits(x,
               'TukeyC'))
    stop("Use only with 'TukeyC' objects!") 

  treat <- eval(getCall(x)$which) 

  if(is.null(xlab)) xlab <- 'Levels' 

  if(inherits(x,'TukeyC.formula')){
    aux2 <- eval(getCall(x)$formula)
    aux3 <- eval(getCall(x)$data)
    response <- as.character(formula(aux2)[[2]])   
  } else if(inherits(x,'TukeyC.aovlist')){
    aux <- eval(getCall(x)$x)
    aux3 <- model.frame(aux)
    response <- as.character(attr(aux,
                                  'terms')[[2]])  
  } else{ 
    aux <- eval(getCall(x)$x)
    aux2 <- eval(getCall(aux)$formula)
    aux3 <- eval(getCall(aux)$data)
    response <- as.character(formula(aux2)[[2]])    
  }

  ltreat <- rownames(x$out$Result)
  means <- x$info$Means[['means']] 

  auxinter <- unlist(strsplit(treat,':'))#objeto criado para auxliar nos casos que envolve interações.

  if(length(auxinter)>1){
    aux3$groups <- with(aux3,
                        interaction(eval(parse(text=treat))))
    aux3$groups <- gsub(':','/',aux3$groups)
    aux3 <- subset(aux3, groups%in%ltreat) 
    treat <- 'groups'
  }

  aux3[[treat]] <- factor(aux3[[treat]],
                          levels = ltreat)

  m.res <- t(x$out$Result[, 2:ncol(x$out$Result)])

  if(dim(m.res)[1] != 1) {
    m.res <- apply(m.res, 2, fun)
    id.groups <- c(apply(m.res,
                         2,
                         paste,
                         collapse=''))
  }
  else{
    id.groups <- m.res 
  }

  aux22 <- as.formula(paste(response,'~',treat))

  ngroups <- dim(x$out$Result)[2] - 1
  if(ngroups > 3){
    op <- par('mar')       # Original par('mar')
    np <- op               # A copy
    np[3] <- ngroups + 1   # Changing top to show all letters
    par(mar=np)            # Setting new par('mar')
  }

  gr <- boxplot(aux22,
                data=aux3,
                xlab = xlab,
                ...)# OK lm class!!! 
  axis(3,
       at     = 1:length(ltreat),
       labels = id.groups, ...)

  #gr$stats[3, ] <- unclass(with(aux3,
  #                              by(aux3[[response]],
  #                                 aux3[[treat]], 
  #                                 function(x) mean(x,na.rm=TRUE)))) 
  gr$stats[3,] <- means

  switch(match.arg(type.mean),
         line = {
           bxp(gr,
               add = TRUE,
               frame.plot = FALSE,
               medcol = col.mean,
               lty = lty.mean,
               lwd = lwd.mean,
               boxlty = 'blank',
               whisklty="blank",
               outlty="blank",
               outpch = NA,
               staplelty="blank",
               show.names=FALSE,
               ...)

           auxlty <- c(1,lty.mean)
           auxpch <- NULL
         },
         point = {
           points(means,
                  col = col.mean,
                  lwd = lwd.mean,
                  pch = pch.mean,
                  ...)

           auxlty <- c(1,NA)
           auxpch <- c(NA,pch.mean)
         },
         none = invisible(NULL))

  if(is.null(args.legend)){

    args.2Kl <- list(x        = 'topleft',
                     legend   = c('Median','Mean'),
                     col = c('black',col.mean),
                     lwd = c(1,lwd.mean),
                     bty = 'n',
                     cex = 0.8,
                     lty = auxlty,
                     pch = auxpch)

    do.call('legend',
            args.2Kl)

  } else {

    args.2Kl <- list(x        = 'topleft',
                     legend   = c('Median','Mean'),
                     col = c('black',col.mean),
                     lwd = c(1,lwd.mean),
                     bty = 'n',
                     cex = 0.8,
                     lty = auxlty,
                     pch = auxpch) 

    args.2Kl[names(args.legend)] <- args.legend     

    do.call('legend',
            args.2Kl) 

  }       

  if(ngroups > 3){
    par(mar=op)  # Restoring the original par('mar') 
  }
}
