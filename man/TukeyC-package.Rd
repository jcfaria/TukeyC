\name{TukeyC-package}
\alias{TukeyC-package}
\docType{package}

\title{
  Conventional Tukey Test}

\description{
  This package perform the Tukey test in the conventional way,
  assigning letters for each group of means, for simple
  experimental designs and schemes. The most usual designs are:
  Completely Randomized Design (CRD),
  Randomized Complete Block Design (RCBD),
  Latin Squares Design (LSD);
  the most usual schemes are:
  Factorial Experiment (FE),
  Split-Plot Experiment (SPE) and
  Split-Split-Plot Experiment (SPE).
  Where it is aplicable, the package can be used for both (balanced or
  unbalanced) experiments.


  \R has some functions
  (\code{\link[stats]{TukeyHSD}} provided by \code{stats},
  \code{\link[multcomp]{glht}} provided by \code{multcomp},
  \code{\link[agricolae]{HSD.test}} provided by \code{agricolae} and
  \code{\link[multcomp]{cld}} provided by \code{multcomp})
  allowing to perform the Tukey test. Some, for exemple \code{TukeyHSD},
  are concerned in the intervals based on the range of the
  sample means rather than the individual differences. The intervals returned
  by these functions are based on this Studentized range statistics.
  This approach has two advantages: the p-value is showed allowing the user to
  flexibilize the inferencial decision and it is possible to plot the result of
  the test. But, has one disadvantage, the final result is more difficult to
  understand and summarize. Others (\code{glht}, \code{gcld}) are very usefful
  for experient statistical. Additionally, the majority of users of other
  statistical softwares are accustomed with letters grouping the means levels
  of the factor tested, making unattractive or difficult to adapt to the current
  aproach of \R.

  So, the main aim of this package is make available in \R environment the
  conventional aproach of Tukey test with a set of flexible funtions and
  S3 methods.}

\details{
  \tabular{ll}{
  Package: \tab TukeyC\cr
  Type: \tab Package\cr
  Version: \tab 1.0-2\cr
  Date: \tab 2011-04-10\cr
  License: \tab GPL (>= 2)\cr}}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr}

\keyword{package}

\examples{
  ##
  ## Examples: Completely Randomized Design (CRD)
  ## More details: demo(package='TukeyC')
  ##
  
  ## The parameters can be: vectors, design matrix and the response variable,
  ## data.frame or aov
  data(CRD2)
  
  ## From: design matrix (dm) and response variable (y) - balanced
  tk1 <- with(CRD2, TukeyC(x=dm, y=y, model='y ~ x',
                           which='x', sig.level=0.05))
  summary(tk1)
  plot(tk1, id.las=2, rl=FALSE)
  
  ## From: design matrix (dm) and response variable (y) - unbalanced
  tk1u <- with(CRD2, TukeyC(x=dm[-1, ], y=y[-1], model='y ~ x',
                            which='x', sig.level=0.05))
  summary(tk1u)
  
  ## From: data.frame (dfm) - balanced
  tk2 <- with(CRD2, TukeyC(x=dfm, model='y ~ x',
                           which='x'))
  summary(tk2)
  
  ## From: data.frame (dfm) - balanced
  tk2u <- with(CRD2, TukeyC(x=dfm[-1, ], model='y ~ x',
                            which='x'))
  summary(tk2u)
  
  ## From: aov - balanced
  av <- with(CRD2, aov(y ~ x , data = dfm))
  summary(av)
  
  tk3 <- with(CRD2, TukeyC(x=av,
                           which='x'))
  summary(tk3)

  ## From: aov - unbalanced
  avu <- with(CRD2, aov(y ~ x , data = dfm[-1, ]))
  summary(avu)
  
  tk3u <- with(CRD2, TukeyC(x=avu,
                            which='x'))
  summary(tk3u)

  ##
  ## Example: Randomized Complete Block Design (RCBD)
  ## More details: demo(package='TukeyC')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(RCBD)
  
  ## Design matrix (dm) and response variable (y)
  tk1 <- with(RCBD, TukeyC(x=dm, y=y, model='y ~ blk + tra',
                           which = 'tra'))
  summary(tk1)
  plot(tk1)
  
  ## From: data.frame (dfm), which='tra'
  tk2 <- with(RCBD, TukeyC(x=dfm, model='y ~ blk + tra',
                           which='tra'))
  summary(tk2)
  
  ##
  ## Example: Latin Squares Design (LSD)
  ## More details: demo(package='TukeyC')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(LSD)
  
  ## From: design matrix (dm) and response variable (y)
  tk1 <- with(LSD, TukeyC(x=dm, y=y, model='y ~ rows + cols + tra',
                          which='tra'))
  summary(tk1)
  plot(tk1)
  
  ## From: data.frame
  tk2 <- with(LSD, TukeyC(x=dfm, model='y ~ rows + cols + tra',
                          which='tra'))
  summary(tk2)
 
  ## From: aov
  av <- with(LSD, aov(y ~ rows + cols + tra, data=dfm))
  summary(av)
  
  tk3 <- TukeyC(av,
                which='tra')
  summary(tk3)

  ##
  ## Example: Factorial Experiment (FE)
  ## More details: demo(package='TukeyC')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(FE)

  ## From: design matrix (dm) and response variable (y)
  ## Main factor: N
  tk1 <- with(FE, TukeyC(x=dm, y=y, model='y ~ blk + N*P*K', 
                         which='N'))
  summary(tk1)
  plot(tk1)

  ## Nested: p1/N
  ntk1 <- with(FE, TukeyC.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                               which='N:P', fl2=1))
  summary(ntk1)

  ## Nested: k1/P
  ntk2 <- with(FE, TukeyC.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                               which='P:K', fl2=1))
  summary(ntk2)

  ## Nested: k2/p2/N
  ntk3 <- with(FE, TukeyC.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                               which='N:P:K', fl2=2, fl3=2))
  summary(ntk3)

  ## Nested: k1/n1/P
  ntk4 <- with(FE, TukeyC.nest(x=dm, y=y, model='y ~ blk + P*N*K',
                               which='P:N:K', fl2=1, fl3=1))
  summary(ntk4)

  ## Nested: p1/n1/K
  ntk5 <- with(FE, TukeyC.nest(x=dm, y=y, model='y ~ blk + K*N*P',
                               which='K:N:P', fl2=1, fl3=1))
  summary(ntk5)

  ##
  ## Example: Split-plot Experiment (SPE)
  ## More details: demo(package='TukeyC')
  ##
  
  data(SPE)

  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  ## From: design matrix (dm) and response variable (y)
  ## Main factor: P 
  tk1 <- with(SPE, TukeyC(x=dm, y=y, model='y ~ blk + SP*P + Error(blk/P)',
                          which='P', error ='blk:P'))
  summary(tk1)
  
  ## Main factor: SP
  tk2 <- with(SPE, TukeyC(x=dm, y=y, model='y ~ blk + SP*P + Error(blk/P)',
                          which='SP', error ='Within', sig.level=0.025 ))
  summary(tk2)
  plot(tk2)
  
  ## Nested: sp/p=1
  tkn1 <- with(SPE, TukeyC.nest(x=dm, y=y, model='y ~ blk + SP*P + Error(blk/P)',
                                which='SP:P', error ='Within', fl2=1 ))
  summary(tkn1)

  ##
  ## Example: Split-split-plot Experiment (SSPE)
  ## More details: demo(package='TukeyC')
  ##
  
  data(SSPE)

  ## From: design matrix (dm) and response variable (y)
  ## Main factor: P
  tk1 <- with(SSPE, TukeyC(dm, y, model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                           which='P', error='blk:P'))
  summary(tk1)
  
  # Main factor: SP
  tk2 <- with(SSPE, TukeyC(dm, y, model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                           which='SP', error='blk:P:SP', sig.level=0.025))
  summary(tk2)
  
  # Main factor: SSP
  tk3 <- with(SSPE, TukeyC(dm, y, model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                           which='SSP', error='Within', sig.level=0.1))
  summary(tk3)
  plot(tk3)
  
  ## Nested: p1/SP
  tkn1 <- with(SSPE, TukeyC.nest(dm, y, model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                                 which='SP:P', error='blk:P:SP', fl2=1))
  summary(tkn1)

  ## From: aovlist
  av <- with(SSPE, aov(y ~  blk + SSP*SP*P + Error(blk/P/SP), data=dfm))
  summary(av)   

  ## Nested:sp/sp/SSP (at various levels of P and SP) 
  tkn6 <- TukeyC.nest(av,
                      which='SSP:SP:P', error='Within', fl2=1, fl3=1)
  summary(tkn6)
  plot(tkn6)

  tkn7 <- TukeyC.nest(av,
                      which='SSP:SP:P', error='Within', fl2=2, fl3=1)
  summary(tkn7)
  plot(tkn7)
}
