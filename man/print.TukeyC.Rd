\name{print.TukeyC}
\alias{print.TukeyC}

\title{
	Print Method for \code{TukeyC} objects.
}

\description{
	Returns (and prints) a list for objects of class \code{TukeyC}.
}

\usage{

\method{print}{TukeyC}(x, ...)

}

\arguments{
	\item{x}{A given object of the class \code{TukeyC}.}
	\item{\dots }{Further arguments (require by generic).}
}

\value{
	A list with the following elements:
	\item{a list of length 5}{In the first position of the list there is a data.frame with the means and the groupings. In the second position of the list there is a scalar with the significance level. In the third position there is a matrix with the p-values obtained in each mean comparison. In the fourth position there is another matrix with the values obtained from the minimum significance difference. In the fifth position there is a vector with the number of repetitions of the experiment.}
	\item{a list of length 5}{In the first position there is a data.frame with the names of the treatments and the means. In the second position there is another data.frame with the means, minimum and maximum of the data. In the third position there are the means with the lower and upper limits of the confidence interval using the standard deviation to calculate the margin of error. In the fourth position there is also a data.frame with the means and the lower and upper limits of a confidence interval using the standard error of the mean of each treatment to calculate the margin of error. In the fifth position there is also a data.frame with the means and the lower and upper limits of the confidence interval using the standard error of the experimental error to calculate the margin of error.}
	\item{a list of length 1}{An object of the type "call".}
}

\author{
  Faria, J. C. (\email{joseclaudio.faria@gmail.com})\cr
  Jelihovschi, E. G. (\email{eniojelihovs@gmail.com})\cr
  Allaman, I. B. (\email{ivanalaman@gmail.com})
}

\seealso{\code{\link{TukeyC}}
}

\examples{
data(RCBD) 

tk <- with(RCBD,
	   TukeyC(y ~ blk + tra,
		  data=dfm,
		  which='tra'))
tk

}

\keyword{package}
\keyword{TukeyC}