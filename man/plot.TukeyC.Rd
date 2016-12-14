\name{plot.TukeyC}
\alias{plot.TukeyC}

\title{
  Plot TukeyC and TukeyC.nest Objects
}

\description{
  S3 method to plot \code{TukeyC} and \code{TukeyC.nest} objects.
}

\usage{
\method{plot}{TukeyC}(x,
     result         = TRUE,
     replicates     = TRUE,
     pch            = 19,
     col            = NULL,
     xlab           = NULL,
     ylab           = NULL,
     xlim           = NULL,
     ylim           = NULL,
     id.lab         = NULL,
     id.las         = 1,
     rl             = TRUE,
     rl.lty         = 3,
     rl.col         = 'gray',
     dispersion     = c('none','mm','sd','ci'),
     dispersion.lty = 1,
     title          = '', \dots)
}

\arguments{
  \item{x}{A \code{TukeyC} object.}
  \item{result}{The result of the test (letters) should be visible.}
  \item{replicates}{The number of replicates should be visible.}
  \item{pch}{A vector of plotting symbols or characters.}
  \item{col}{A vector of colors for the means representation.}
  \item{xlab}{A label for the \samp{x} axis.}
  \item{ylab}{A label for the \samp{y} axis.}
  \item{xlim}{The \samp{x} limits of the plot.}
  \item{ylim}{The \samp{y} limits of the plot.}
  \item{id.lab}{Factor level names at \samp{x} axis.}
  \item{id.las}{Factor level names written either horizontally or vertically.}
  \item{rl}{Horizontal line connecting the circle to the \samp{y} axis.}
  \item{rl.lty}{Line type of \samp{rl}.}
  \item{rl.col}{Line color of \samp{rl}.}
  \item{dispersion}{Vertical line through the circle (mean value) linking the minimum to the maximum of the factor level values corresponding to that mean value. Other options are: sd (standard deviation), ci (confidence interval) and none.}
  \item{dispersion.lty}{Line type of dispersion.}
  \item{title}{A title for the plot.}
  \item{\dots}{Optional plotting parameters.}
}

\details{
  The \code{plot.TukeyC} function is a S3 method to plot \samp{Tukey} and
  \code{TukeyC.nest} objetcs. It generates a serie of points (the means) and a 
  vertical line showing the dispersion of the values corresponding to
  each group mean. 
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
  \code{\link{plot}}
}

\examples{
##
## Examples: Completely Randomized Design (CRD)
## More details: demo(package='TukeyC')
##

library(TukeyC)
data(CRD2)

## From: formula
tk1 <- with(CRD2,
            TukeyC(y ~ x,
                   data=dfm,
                   which='x'))

old.par <- par(mar=c(6, 3, 6, 2))
plot(tk1,
     id.las=2)

plot(tk1,
     dispersion='sd',
     mm.lty=3,
     id.las=2,
     rl=FALSE)

## From: aov
av <- with(CRD2,
           aov(y ~ x,
               data=dfm))
summary(av)

tk2 <- TukeyC(x=av,
              which='x')
plot(tk2,
     rl=FALSE,
     id.las=2)

# From: lm
av_lm <- with(CRD2,
           lm(y ~ x,
              data=dfm))

tk3 <- TukeyC(x=av_lm,
              which='x')
plot(tk3,
     dispersion='ci',
     id.las=2,
     rl=FALSE)

par(old.par)
}

\keyword{package}

