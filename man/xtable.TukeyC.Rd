\name{xtable.TukeyC}
\alias{xtable.TukeyC}

\title{
  xtable method for \code{TukeyC} objects.
}

\description{
  Convert an \code{TukeyC} object to an \code{xtable.TukeyC} object, which can then be printed as a LaTeX or HTML table. This function is an additional method to xtable function of xtable package.
}

\usage{

  \method{xtable}{TukeyC}(x, ...)

}

\arguments{
  \item{x}{A given object of the class \code{TukeyC}.}
  \item{\dots}{Further arguments (require by xtable::xtable).}
}

\value{
  For most \code{xtable} methods, an object of class "xtable.TukeyC", "xtable" which inherits the data.frame class and contains several additional attributes specifying the table formatting options.
}

\author{
  Faria, J. C. (\email{joseclaudio.faria@gmail.com})\cr
  Jelihovschi, E. G. (\email{eniojelihovs@gmail.com})\cr
  Allaman, I. B. (\email{ivanalaman@gmail.com})
}

\seealso{
 \code{\link[xtable:xtable]{xtable}}
}

\examples{
data(RCBD) 
\donttest{
  if(require(xtable)){
    lm1 <- with(RCBD,
                  lm(y ~ blk + tra,
                       data=dfm))

    tk1 <-  TukeyC(lm1,
                   which='tra')
    tb <- xtable(tk1)
    print(tb)
  }
 } 
}

\keyword{package}
\keyword{TukeyC}
