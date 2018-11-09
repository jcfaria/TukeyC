\name{boxplot.TukeyC}
\alias{boxplot.TukeyC}

\title{
  Boxplot TukeyC Objects
}

\description{
  S3 method to plot \code{TukeyC} objects.
}

\usage{
\method{boxplot}{TukeyC}(x,
          type.mean   = c('line','point','none'),
          xlab        = NULL,
          col.mean    = 'gray',
          pch.mean    = 1,
          lwd.mean    = 1,
          lty.mean    = 1, 
          args.legend = NULL,\dots)
}

\arguments{
  \item{x}{A \code{TukeyC} object.}
  \item{type.mean}{The type of mean must be ploted. O default is "line".}
  \item{xlab}{A label for the \samp{x} axis.} 
  \item{col.mean}{A vector of colors for the means representation.} 
  \item{pch.mean}{A vector of plotting symbols or characters. Only if type are "point".}
  \item{lwd.mean}{Line width of mean.} 
  \item{lty.mean}{Line type of mean. Only if type are "line".}
  \item{args.legend}{List of additional arguments to be passed to \code{legend}; The default is \code{NULL}.}
  \item{\dots}{Optional plotting parameters.}
}

\details{
  The \code{boxplot.TukeyC} function is a S3 method to plot \samp{TukeyC} objetcs. The difference to generic function is the Tukey inference under frame and the plot of means within box.
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\references{
  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}

\seealso{
  \code{\link{boxplot}}
}

\examples{
##
## Examples: Completely Randomized Design (CRD)
## More details: demo(package='TukeyC')
##

library(TukeyC)
data(CRD1)

## From: formula
# Simple!
tk1 <- TukeyC(y ~ x,
              data=CRD1$dfm,
              which='x')
boxplot(tk1)

# A little more elaborate!
boxplot(tk1,
        lwd.mean = 1.3,
        col.mean = 'red')

# A little more! 
boxplot(tk1,
        lwd.mean = 1.3,
        lty.mean = 2,
        col.mean = 'red',
        args.legend = list(x='bottomleft'))

# With point type!
boxplot(tk1,
        type = 'point')

# With other point
boxplot(tk1,
        type = 'point',
        pch.mean = 3,
        col.mean = 'blue',
        args.legend = list(x='bottomleft')) 

}

\keyword{package}

