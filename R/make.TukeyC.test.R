##
## Function to perform Tukey test
##

make.TukeyC.test <-
  function(r=r, MSE=MSE, m.inf=m.inf, ord=ord, sig.level=sig.level, dfr=dfr,
           bal=bal, mt=mt, round=round)
  {
    if (length(r) < nrow(m.inf))        # expand n to the correct length if necessary
      r <- rep.int(r, nrow(m.inf))
    r <- r[ord] 

    m.tmp <- m.inf[, 1]
    names(m.tmp) <- r

    vece <- outer(X=m.tmp, Y=m.tmp,     # (v)ariance (e)stimation of (c)ontrast (e)stimation
                  function(X, Y) MSE * (1/as.numeric(names(X)) + (1/as.numeric(names(Y)))))

    qTukey <- qtukey(1 - sig.level, nrow(m.inf), dfr)

    if (!bal) {
      msd <- qTukey * sqrt(1/2 * vece)  # minimum significative difference
      diag(msd) <- 0
      dimnames(msd) <- list(rownames(m.inf), rownames(m.inf))}
    else
      msd <- qTukey * sqrt(1/2 * vece)[1,1]

    m    <- m.inf[,1]
    difm <- abs(outer(m, m, "-"))       # means difference
    dif  <- difm - msd
    dif  <- ifelse(dif <= 0, FALSE, TRUE)
    res  <- make.TukeyC.groups(dif)
    res  <- cbind(format(round(m, round), nsmall=2), res)
    colnames(res) <- c('Means', paste('G', 1:(ncol(res) - 1), sep=''))

    diag(difm) <- 0
    if (bal) r <- r[1]

    out <- list(Table       = mt,
                Means       = m.inf,
                Result      = as.data.frame(res),
                Sig.level   = sig.level,
                Differences = round(difm, 2),
                MSD         = round(msd, 2),
                Replicates  = r)
    return(out)
  }
