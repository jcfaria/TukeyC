\name{xtable.TukeyC}
\alias{xtable.TukeyC}
\alias{print.xtable.TukeyC}

\title{
  xtable method for \code{TukeyC} objects.
}

\description{
  Convert a \code{TukeyC} object to an \code{xtable.TukeyC} object, which can then be printed as a LaTeX or HTML table. This function provides an additional method for the \code{xtable} function from the \pkg{xtable} package.
}

\usage{

\method{xtable}{TukeyC}(x, ...)

\method{print}{xtable.TukeyC}(x, include.rownames = FALSE, ...)

}

\arguments{
  \item{x}{A given object of the class \code{TukeyC} (for \code{xtable}) or \code{xtable.TukeyC} (for \code{print}).}
  \item{include.rownames}{Logical; passed to \code{xtable::print.xtable}. Treatment names are already in the \code{Treatment} column, so the default is \code{FALSE}.}
  \item{\dots}{Further arguments (required by \code{xtable::xtable} or \code{xtable::print.xtable}).}
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

lm1 <- with(RCBD,
            lm(y ~ blk + tra,
               data = dfm))

tk1 <- TukeyC(lm1,
              which = 'tra')

tb <- xtable(tk1)
\dontrun{
  print(tb)
}
}

\keyword{package}
