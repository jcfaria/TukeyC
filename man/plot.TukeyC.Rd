\name{plot.TukeyC}
\alias{plot.TukeyC}

\title{
  Plot TukeyC and TukeyC.nest Objects}

\description{
  S3 method to plot \code{TukeyC} and \code{TukeyC.nest} objects.}

\usage{
  \method{plot}{TukeyC}(x,
     result=TRUE,
     replicates=TRUE,
     pch=19,
     col=NULL,
     xlab=NULL,
     ylab=NULL,
     xlim=NULL,
     ylim=NULL,
     id.lab=NULL,
     id.las=1,
     rl=TRUE,
     rl.lty=3,
     rl.col='gray',
     mm=TRUE,
     mm.lty=1,
     title='', \dots)}

\arguments{
  \item{x}{A \code{TukeyC} object.}
  \item{result}{The result of the test (letters) should be visible.}
  \item{replicates}{The number of replicates should be visible.}
  \item{pch}{A vector of plotting symbols or characters.}
  \item{col}{A vector of colors for the means representation.}
  \item{xlab}{A label for the x axis.}
  \item{ylab}{A label for the y axis.}
  \item{xlim}{The x limits of the plot}.
  \item{ylim}{The y limits of the plot.}
  \item{id.lab}{Factor level names at x-axis.}
  \item{id.las}{Factor level names written either horizontally or vertically.}
  \item{rl}{Horizontal line connecting the circle to the y-axis.}
  \item{rl.lty}{Line type of rl.}
  \item{rl.col}{Line color of rl.}
  \item{mm}{Vertical line through the circle (mean value) linking the minimum to the maximum of the factor level values corresponding to that mean value.}
  \item{mm.lty}{Line type of mm.}
  \item{title}{A title for the plot.}
  \item{\dots}{Optional plotting parameters.}} 

\details{
  The \code{plot.TukeyC} function is a S3 method to plot \code{Tukey} and
  \code{TukeyC.nest} objetcs. It generates a serie of points (the means) and a 
  vertical line showing the minimum e maximum of the values corresponding to
  each group mean. The groups are diferentiated by colors.}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr}

\references{
  Murrell, P. (2005) R Graphics. Chapman & Hall/CRC Press.}

\seealso{\code{\link{plot}}}

\examples{
  ##
  ## Examples: Completely Randomized Design (CRD)
  ## More details: demo(package='TukeyC')
  ##

  library(TukeyC)
  data(CRD2)

  ## From: vectors x and y
  tk1 <- with(CRD2,
              TukeyC(x=x,
                     y=y,
                     model='y ~ x',
                     which='x'))
  plot(tk1, id.las=2, rl=FALSE)

  ## From: design matrix (dm) and response variable (y)
  tk2 <- with(CRD2,
              TukeyC(x=dm,
                     y=y,
                     model='y ~ x',
                     which='x',
                     sig.level=0.005))
  plot(tk2, mm.lty=3, id.las=2, rl=FALSE)

  ## From: data.frame (dfm)
  tk3 <- with(CRD2,
              TukeyC(x=dfm,
                     model='y ~ x',
                     which='x'))
  plot(tk3, id.las=2, rl=FALSE)

  ## From: aov
  av <- with(CRD2,
             aov(y ~ x,
             data=dfm))
  summary(av)

  tk4 <- with(CRD2,
              TukeyC(x=av,
                     which='x'))
  plot(tk4, rl=FALSE, id.las=2)
}

\keyword{package}

