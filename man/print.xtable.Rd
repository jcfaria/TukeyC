\name{print.xtable}
\alias{print.xtable.default}
\alias{print.xtable}

\title{
  Print method for \code{xtable} objects.
}

\description{
  Returns (and prints) a list for objects of class \code{xtable}.
}

\usage{

\method{print}{xtable}(x, ...)

}

\arguments{
  \item{x}{A given object of the class \code{xtable}.}
  \item{\dots }{Further arguments (require by generic).}
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr  
  Enio G. Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr
}

\seealso{\code{\link{print.xtable}}
}

\examples{
data(RCBD) 

tk <- with(RCBD,
           TukeyC(y ~ blk + tra,
                  data=dfm,
                  which='tra'))
## From: xtable
## This class is specific of the xtable package.
\dontrun{
  if(require(xtable)){
    lm1 <- with(RCBD,
                  lm(y ~ blk + tra,
                       data=dfm))

    tk1 <-  TukeyC(lm1,
                   which='tra')
    print(xtable(tk1))
  }
 }
}

\keyword{package}
\keyword{TukeyC}
