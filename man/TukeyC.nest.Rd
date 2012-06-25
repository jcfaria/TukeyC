\name{TukeyC.nest}
\alias{TukeyC.nest}
\alias{TukeyC.nest.default}
\alias{TukeyC.nest.aov}
\alias{TukeyC.nest.aovlist}

\title{
  The TukeyC ... for Factorial, Split-plot and Split-SPlit
    plot Experiments}

\description{
  These are methods for objects of class \code{vector}, \code{matrix} or
  \code{data.frame} joined as default, \code{aov} and \code{aovlist} for
  factorial, split-plot and split-split-plot experiments.}

\usage{
  \method{TukeyC.nest}{default}(x,
            y=NULL,
            model,
            which,
            error,
            fl2,
            fl3=0,
            sig.level=.05,
            round=2, \dots)

  \method{TukeyC.nest}{aov}(x,
            which,
            fl2,
            fl3=0,
            sig.level=.05,
            round=2, \dots)

  \method{TukeyC.nest}{aovlist}(x,
            which,
            error,
            fl2,
            fl3=0,
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
  \item{fl2}{A vector of length 1 giving the level of the second factor in nesting order tested.}
  \item{fl3}{A vector of length 1 giving the level of the third factor in nesting order tested.}
  \item{error}{The error to be considered, only in case of split-plots experiments.}
  \item{sig.level}{Level of Significance used in the TukeyC algorithm to create
    the groups of means. The default value is 0.05.}
  \item{round}{Integer indicating the number of decimal places.}
  \item{\dots}{Potential further arguments (required by generic).}}

\details{
  The function \code{TukeyC.nest} returns an object of class 
  \code{TukeyC.nest} containing the groups of means plus other
  necessary variables for summary and plot.
  
  The generic functions \code{summary} and \code{plot} are used to obtain and
  print a summary and a plot of the results.}

\value{
  The function \code{TukeyC.nest} returns a list of the class \code{TukeyC.nest} with the slots:
  \item{av}{A \code{list} storing the result of \code{aov}.}
  \item{groups}{A vector of length equal the number of factor levels marking the groups generated.}
  \item{nms}{A vector of the labels of the factor levels.}
  \item{ord}{A vector which keeps the position of the means of the factor levels in decreasing order.}
  \item{m.inf}{A matrix which keeps the means, minimum and maximum of the factor levels in decreasing order.}
  \item{sig.level}{A vector of length 1 giving the level of significance of the test.}
  \item{r}{A vector of length 1 giving the number of replicates.}
  \item{which}{The name of the factor whose levels were tested.}
  \item{tab}{An array keeping the names of the factors and factor levels and also the mean value of the repetitions for every combination of factor levels.} 
  \item{fl2}{A vector of length 1 giving the level of the second factor in nesting order tested.}
  \item{fl3}{A vector of length 1 giving the level of the third factor in nesting order tested.}}

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
  ## Example: Split-split-plot Experiment (SSPE)
  ## More details: demo(package='TukeyC')
  ##
  
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
  
  ## Nested: p1/SP
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

  ## Nested: p/sp/SSP (at various levels of sp and p) 
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

