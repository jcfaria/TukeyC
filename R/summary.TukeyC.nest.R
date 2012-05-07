##
## S3 method to sumarize 'TukeyC.nest' object
##

summary.TukeyC.nest <-
  function(object, ...)
  {
    if(!inherits(object, 'TukeyC'))
      stop("Use only with \"TukeyC\" objects!")
    res <- object$Result
    res
  }
