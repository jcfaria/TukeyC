##
## S3 method to sumarize 'SK' object
##

summary.TukeyC <- 
  function(object, ...)
  {
    if(!inherits(object, 'TukeyC'))
      stop("Use only with \"TukeyC\" objects!")
    res <- object$Result
    res
  }                   
