\name{TukeyC-package}
\alias{TukeyC-package}
\docType{package}

\title{
  Conventional Tukey Test
}

\description{
  This package performs what is known as the Tukey HSD test in the conventional 
  way. It also uses an algorithm which divides the set of all means in groups 
  and assigns letters to the different groups, allowing for overlapping. This is 
  done for simple experimental designs and schemes. The most usual designs are:
  Completely Randomized Design (\samp{CRD}),
  Randomized Complete Block Design (\samp{RCBD}) and 
  Latin Squares Design (\samp{LSD}).
  The most usual schemes are:
  Factorial Experiment (\samp{FE}),
  Split-Plot Experiment (\samp{SPE}) and
  Split-Split-Plot Experiment (\samp{SPE}).
  
  The package can be used for both balanced or unbalanced (when possible), experiments.

  \R has some functions
  (\code{\link[stats]{TukeyHSD}} provided by \code{stats},
  \code{\link[multcomp]{glht}} provided by \code{multcomp},
  \code{\link[agricolae]{HSD.test}} provided by \code{agricolae} and
  \code{\link[multcomp]{cld}} provided by \code{multcomp}) which also performs
  the Tukey test. The \code{TukeyHSD} returns intervals based on the range of the
  sample means rather than the individual differences. Those intervals are based 
  on Studentized range statistics and are, in essence, confidence intervals.
  This approach has two advantages: the p-value is showed allowing the user to
  flexibilize the inferencial decision and also make it possible to plot the 
  result of the test. However, it has one disadvantage, since the final result is 
  more difficult to understand and summarize. Others (\code{glht}, \code{cld}) 
  are also useful but difficult to manage.
  Additionally, most of users of other statistical softwares are very used with 
  letters grouping the means of the factor tested, making unattractive or 
  difficult to adapt to the current aproach of \R.

  So, the main aim of this package is make available in \R environment the
  conventional aproach of Tukey test with a set of flexible funtions and
  S3 methods.
}
  
\references{
  Miller, R.G. (1981) \emph{Simultaneous Statistical Inference}. Springer.

  Ramalho M.A.P, Ferreira D.F & Oliveira A.C. (2000) \emph{Experimentação em Genética
  e Melhoramento de Plantas}. Editora UFLA.

  Steel, R.G., Torrie, J.H & Dickey D.A. (1997) \emph{Principles and procedures of statistics:
  a biometrical approach}. Third Edition.
  
  Yandell, B.S. (1997) \emph{Practical Data Analysis for Designed Experiments}. 
  Chapman & Hall.
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

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
  tk1 <- with(CRD2,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ x',
                     which='x'))
  summary(tk1)
  plot(tk1,
       id.las=2,
       rl=FALSE)
  
  ## From: design matrix (dm) and response variable (y) - unbalanced
  tk1u <- with(CRD2,
               TukeyC(x=dm[-1,],
                      y=y[-1],
                      model='y ~ x',
                      which='x',
                      dispersion='s'))
  summary(tk1u)
  plot(tk1u)
  
  ## From: data.frame (dfm) - balanced
  tk2 <- with(CRD2,
              TukeyC(x=dfm,
                     model='y ~ x',
                     which='x',
                     dispersion='se'))
  summary(tk2)
  plot(tk2)
  
  ## From: data.frame (dfm) - balanced
  tk2u <- with(CRD2,
               TukeyC(x=dfm[-1,],
                      model='y ~ x',
                      which='x'))
  summary(tk2u)
  
  ## From: aov - balanced
  av <- with(CRD2,
             aov(y ~ x,
             data=dfm))
  summary(av)
  
  tk3 <- TukeyC(x=av,
                which='x')
  summary(tk3)

  ## From: aov - unbalanced
  avu <- with(CRD2,
              aov(y ~ x,
              data=dfm[-1,]))
  summary(avu)
  
  tk3u <- TukeyC(x=avu,
                 which='x')
  summary(tk3u)

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
  plot(tk1)
  
  ## From: data.frame (dfm), which='tra'
  tk2 <- with(RCBD,
              TukeyC(x=dfm,
                     model='y ~ blk + tra',
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
  tk1 <- with(LSD,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ rows + cols + tra',
                     which='tra'))
  summary(tk1)
  plot(tk1)
  
  ## From: data.frame
  tk2 <- with(LSD,
              TukeyC(x=dfm,
                     model='y ~ rows + cols + tra',
                     which='tra'))
  summary(tk2)
 
  ## From: aov
  av <- with(LSD,
             aov(y ~ rows + cols + tra,
             data=dfm))
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
  tk1 <- with(FE,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ blk + N*P*K', 
                     which='N'))
  summary(tk1)
  plot(tk1)

  ## Nested: p1/N
  ## Testing N inside of level one of P
  ntk1 <- with(FE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + N*P*K',
                           which='P:N',
                           fl1=1))
  summary(ntk1)

  ## Nested: k1/P
  ntk2 <- with(FE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + N*P*K',
                           which='K:P',
                           fl1=1))
  summary(ntk2)

  ## Nested: k1/p1/N
  ## Testing N inside of level one of K and level one of P
  ntk3 <- with(FE, 
               TukeyC.nest(x=dm,
                           y=y, 
                           model='y ~ blk + N*P*K',
                           which='K:P:N',
                           fl1=1,
                           fl2=1))
  summary(ntk3)

  ## Nested: k2/n2/P
  ntk4 <- with(FE, 
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + N*P*K',
                           which='K:N:P',
                           fl1=2,
                           fl2=2))
  summary(ntk4)

  ## Nested: p1/n1/K
  ntk5 <- with(FE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + N*P*K',
                           which='P:N:K',
                           fl1=1,
                           fl2=1))
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
  tk1 <- with(SPE,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ blk + P*SP + Error(blk/P)',
                     which='P',
                     error='blk:P'))
  summary(tk1)
  
  ## Main factor: SP
  tk2 <- with(SPE,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ blk + P*SP + Error(blk/P)',
                     which='SP',
                     error='Within'))
  summary(tk2)
  plot(tk2)
  
  ## Nested: p=1/sp
  tkn1 <- with(SPE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + P*SP + Error(blk/P)',
                           which='P:SP',
                           error='Within',
                           fl1=1 ))
  summary(tkn1)

  ##
  ## Example: Split-split-plot Experiment (SSPE)
  ## More details: demo(package='TukeyC')
  ##
  
  data(SSPE)

  ## From: design matrix (dm) and response variable (y)
  ## Main factor: P
  tk1 <- with(SSPE,
              TukeyC(dm,
                     y,
                     model='y ~ blk + P*SP*SSP + Error(blk/P/SP)',
                     which='P',
                     error='blk:P'))
  summary(tk1)
  
  # Main factor: SP
  tk2 <- with(SSPE,
              TukeyC(dm,
                     y,
                     model='y ~ blk + P*SP*SSP + Error(blk/P/SP)',
                     which='SP',
                     error='blk:P:SP'))
  summary(tk2)
  
  # Main factor: SSP
  tk3 <- with(SSPE,
              TukeyC(dm,
                     y,
                     model='y ~ blk + P*SP*SSP + Error(blk/P/SP)',
                     which='SSP',
                     error='Within'))
  summary(tk3)
  plot(tk3)
  
  ## Nested: p1/SP
  tkn1 <- with(SSPE,
               TukeyC.nest(dm,
                           y,
                           model='y ~ blk + P*SP*SSP + Error(blk/P/SP)',
                           which='P:SP',
                           error='blk:P:SP',
                           fl1=1))
  summary(tkn1)

  ## From: aovlist
  av <- with(SSPE,
             aov(y ~  blk + P*SP*SSP + Error(blk/P/SP),
             data=dfm))
  summary(av)   

  ## Nested: p1/sp1/SSP
  ## Testing SSP inside of level one of P and level one of SP
  tkn6 <- TukeyC.nest(av,
                      which='P:SP:SSP',
                      error='Within',
                      fl1=1,
                      fl2=1)
  summary(tkn6)
  plot(tkn6)

  ## Nested: p2/sp1/SSP
  tkn7 <- TukeyC.nest(av,
                      which='P:SP:SSP',
                      error='Within',
                      fl1=2,
                      fl2=1)
  summary(tkn7)
  plot(tkn7)
}
