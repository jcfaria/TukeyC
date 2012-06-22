\name{summary}
\alias{summary.TukeyC}

\title{
  Summary Method for TukeyC and TukeyC.nest Objects}

\description{
  Returns (and prints) a summary list for \code{TukeyC} objects.}

\usage{
  \method{summary}{TukeyC}(object,
        complete=TRUE, \dots)}

\arguments{
  \item{object}{A given object of the class \code{TukeyC}.}
  \item{complete}{A logical value indicating if the summary is complete (mean 
  difference and p-value) or only the groups.}
  \item{\dots}{Potential further arguments (required by generic).}}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})}

\references{
  Chambers, J.M. and Hastie, T.J. (1992) \emph{Statistical Models in S}.
  Wadsworth & Brooks/Cole.}

\seealso{\code{\link{TukeyC}}}

\examples{
  ##
  ## Examples: Completely Randomized Design (CRD)
  ## More details: demo(package='TukeyC')
  ##
  
  ## The parameters can be: vectors, design matrix and the response variable,
  ## data.frame or aov
  data(CRD2)
  
  ## From: design matrix (dm) and response variable (y)
  tk1 <- with(CRD2,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ x',
                     which='x',
                     id.trim=5))
  summary(tk1)

  ##
  ## Example: Randomized Complete Block Design (RCBD)
  ## More details: demo(package='TukeyC')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(RCBD)
  
  ## Design matrix (dm) and response variable (y)
  tk1 <- with(RCBD,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ blk + tra',
                     which='tra'))
  summary(tk1)

  ##
  ## Example: Latin Squares Design (LSD)
  ## More details: demo(package='TukeyC')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
       
  data(LSD)
  
  ## From: design matrix (dm) and response variable (y)
  tk1 <- with(LSD,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ rows + cols + tra',
                     which='tra'))
  summary(tk1)

  ##
  ## Example: Factorial Experiment (FE)
  ## More details: demo(package='TukeyC')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(FE)
  ## From: design matrix (dm) and response variable (y)
  ## Main factor: N
  tk1 <- with(FE,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ blk + N*P*K', 
                     which='N'))
  summary(tk1)

  ## Nested: p1/N
  ntk1 <- with(FE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + N*P*K',
                           which='N:P',
                           fl2=1))
  summary(ntk1)

  ## Nested: k2/p2/N
  ntk2 <- with(FE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + N*P*K',
                           which='N:P:K',
                           fl2=2,
                           fl3=2))
  summary(ntk2)

  ## Nested: k1/n1/P
  ntk3 <- with(FE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + P*N*K',
                           which='P:N:K',
                           fl2=1, 
                           fl3=1))
  summary(ntk3)

  ## Nested: p1/n1/K
  ntk4 <- with(FE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + K*N*P',
                           which='K:N:P',
                           fl2=1,
                           fl3=1))
  summary(ntk4)

  ##
  ## Example: Split-plot Experiment (SPE)
  ## More details: demo(package='TukeyC')
  ##
  
  data(SPE)

  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  ## From: design matrix (dm) and response variable (y)
  ## Main factor: P
  tk1 <- with(SPE,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ blk + SP*P + Error(blk/P)',
                     which='P',
                     error='blk:P'))
  summary(tk1)

  ## Nested: p1/SSP
  tkn1 <- with(SPE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + SP*P + Error(blk/P)',
                           which='SP:P',
                           error='Within',
                           fl2=1 ))
  summary(tkn1)

  data(SSPE)

  ## From: design matrix (dm) and response variable (y)
  ## Main factor: P
  tk1 <- with(SSPE,
              TukeyC(dm,
                     y,
                     model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                     which='P',
                     error='blk:P'))
  summary(tk1)
  
  # Main factor: SP
  tk2 <- with(SSPE,
              TukeyC(dm,
                     y,
                     model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                     which='SP',
                     error='blk:P:SP'))
  summary(tk2)
  
  # Main factor: SSP
  tk3 <- with(SSPE,
              TukeyC(dm,
                     y,
                     model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                     which='SSP',
                     error='Within'))
  summary(tk3)
  
  ## Nested: p1/SSP
  tkn1 <- with(SSPE,
               TukeyC.nest(dm,
                           y,
                           model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                           which='SP:P',
                           error='blk:P:SP',
                           fl2=1))
  summary(tkn1)

  ## From: aovlist
  av <- with(SSPE,
             aov(y ~  blk + SSP*SP*P + Error(blk/P/SP),
             data=dfm))
  summary(av)   

  ## Nested: pp/sp/SSP (at various levels of sp and p) 
  tkn2 <- TukeyC.nest(av,
                      which='SSP:SP:P',
                      error='Within', 
                      fl2=1,
                      fl3=1)
  summary(tkn2)

  tkn3 <- TukeyC.nest(av,
                      which='SSP:SP:P',
                      error='Within',
                      fl2=2,
                      fl3=1)
  summary(tkn3)
}

\keyword{package}

