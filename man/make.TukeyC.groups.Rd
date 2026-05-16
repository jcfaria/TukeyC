\name{make.TukeyC.groups}
\alias{make.TukeyC.groups}

\title{
  Make Tukey Groups
}

\description{
  Builds groups of means, according to the method of Tukey.
}

\usage{
  make.TukeyC.groups(x)
}

\arguments{
  \item{x}{A square matrix whose rows and columns are all levels of the
    factor being tested, sorted in decreasing order. Matrix entries are
    \code{TRUE} or \code{FALSE}, indicating whether the corresponding pair
    of means is not significantly different.}
}

\details{
  This function is an algorithm used to build up the groups of means, 
  allowing for overlapping.
}

\value{
  The object returned by this function is a character matrix and the
  rownames are the levels of the factor being tested, sorted in decreasing
  order. The matrix has the same number of columns as the number of groups
  generated.
}

\note{
  This function is mainly for internal use in the TukeyC package.
}

\author{
  Faria, J. C. (\email{joseclaudio.faria@gmail.com})\cr
  Jelihovschi, E. G. (\email{eniojelihovs@gmail.com})\cr
  Allaman, I. B. (\email{ivanalaman@gmail.com})
}

\keyword{package}
