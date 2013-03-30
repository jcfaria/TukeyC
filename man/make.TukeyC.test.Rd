\name{make.TukeyC.test}
\alias{make.TukeyC.test}

\title{
  Make Tukey Test
}

\description{
  This function implements the Tukey test for balanced or unbalanced designs and schemes.
}

\usage{
  make.TukeyC.test(r=r,
                   MSE=MSE,
                   m.inf=m.inf,
                   ord=ord,
                   sig.level=sig.level,
                   dfr=dfr,
                   bal=bal,
                   mt=mt,
                   round=round)
}

\arguments{
  \item{r}{A vector of the number of repicates of each level of the factor being tested.}
  \item{MSE}{A vector of length 1 giving the mean squared error.}
  \item{m.inf}{A \code{matrix} of the levels of the factor being tested in decreasing order.}
  \item{ord}{A vector of ordered levels of the factor being tested.} 
  \item{sig.level}{A vector of length 1 giving the level of significance of the test.} 
  \item{dfr}{A vector of length 1 giving the degrees of freedom of \samp{MSE}.}
  \item{bal}{A vector of length 1 giving the information wheter the experiment is or not balanced.}
  \item{mt}{The model table.}
  \item{round}{Integer indicating the number of decimal places.}
}

\value{
  A list with 7 slots containing the most important results of the test performed:
  \item{Table}{Table of means of the factors}
  \item{Means}{The means of the factors}
  \item{Result}{The result of the Tukey test}
  \item{Sig.Level}{The significance of the test}
  \item{Diff_Prob}{A \code{matrix} with the observed means differences (\code{upper.tri}) and respective probalities (\code{lower.tri})}
  \item{MSD}{The minimum significative difference}
  \item{Replicates}{The replicate number for each factor}
}

\note{
  This function is mainly for internal use in the \pkg{TukeyC} package.
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\keyword{package}
