\name{TukeyC}
\alias{TukeyC}
\alias{TukeyC.default}
\alias{TukeyC.aov}
\alias{TukeyC.aovlist}

\title{
  The TukeyC Test for Single Experiments}

\description{
  These are methods for objects of class \code{vector}, \code{matrix} or
  \code{data.frame} joined as default, \code{aov} and \code{aovlist} for
  single experiments.}

\usage{
  \method{TukeyC}{default}(x,
       y=NULL, 
       model,
       which,
       error,
       sig.level=.05,
       round=2, \dots)

  \method{TukeyC}{aov}(x,
       which=NULL,
       sig.level=.05,
       round=2, \dots)

  \method{TukeyC}{aovlist}(x,
       which,
       error,
       sig.level=.05, 
       round=2, \dots)}

\arguments{
  \item{x}{A design matrix, \code{data.frame} or an \code{aov} object.}
  \item{y}{A vector of response variable. It is necessary to inform this
    parameter only if \code{x} represent the design matrix.}
  \item{which}{The name of the treatment to be used in the comparison.
    The name must be inside quoting marks.}
  \item{model}{If x is a \code{data.frame} object, the model to be used in the
    aov must be specified.}
  \item{error}{The error to be considered.}
  \item{sig.level}{Level of Significance used in the TukeyC algorithm to create
    the groups of means. The default value is 0.05.}
  \item{round}{Integer indicating the number of decimal places.}
  \item{\dots}{Potential further arguments (required by generic).} }

\details{
  The function \code{TukeyC} returns an object of class \code{TukeyC} 
  respectivally containing the groups of means plus other
  necessary variables for summary and plot.
  
  The generic functions \code{summary} and \code{plot} are used to obtain and
  print a summary and a plot of the results.}

\value{
  The function \code{TukeyC} returns a list of the class \code{TukeyC} with the slots:
  \item{av}{A \code{list} storing the result of \code{aov}.}
  \item{groups}{A vector of length equal the number of factor levels marking the groups generated.}
  \item{nms}{A vector of the labels of the factor levels.}
  \item{ord}{A vector which keeps the position of the means of the factor levels in decreasing order.}
  \item{m.inf}{A matrix which keeps the means, minimum and maximum of the factor levels in decreasing order.}
  \item{sig.level}{A vector of length 1 giving the level of significance of the test.}}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})}

\references{
  Miller, R.G. (1981) \emph{Simultaneous Statistical Inference}. Springer.

  Ramalho M.A.P, Ferreira D.F & Oliveira A.C. (2000) \emph{Experimentacao em Genetica
  e Melhoramento de Plantas}. Editora UFLA.

  Steel, R.G., Torrie, J.H & Dickey D.A. (1997) \emph{Principles and procedures of statistics:
  a biometrical approach}. Third Edition.
  
  Yandell, B.S. (1997) \emph{Practical Data Analysis for Designed Experiments}. 
  Chapman & Hall.}

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
  
  ## From: data.frame (dfm)
  tk2 <- with(CRD2,
              TukeyC(x=dfm,
              model='y ~ x',
              which='x',
              id.trim=5))
  summary(tk2)
  
  ## From: aov
  av <- with(CRD2,
             aov(y ~ x,
             data=dfm))
  summary(av)
  
  tk3 <- with(CRD2,
              TukeyC(x=av,
                     which='x',
                     id.trim=5))
  summary(tk3)

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

  ## Nested: p1/N
  ntk1 <- with(FE,
               TukeyC.nest(x=dm,
                           y=y,
                           model='y ~ blk + N*P*K',
                           which='P:N',
                           fl1=1))
  summary(ntk1)
}

\keyword{package}

