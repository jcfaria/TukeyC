\name{make.TukeyC.test}
\alias{make.TukeyC.test}

\title{
  Make Tukey Test}

\description{
  This function implements the Tukey test for balanced or unbalanced designs
  and schemes.}

\usage{
  make.TukeyC.test(r=r, MSE=MSE, m.inf=m.inf, ord=ord, sig.level=sig.level, dfr=dfr,
    bal=bal, mt=mt, round=round)}

\arguments{
  \item{r}{A vector of the number of repicates of each level of the factor
    being tested.}
  \item{MSE}{A vector of length 1 giving the hean squared error.}
  \item{m.inf}{A matrix of the levels of the factor being tested in
    decreasing order.}
  \item{ord}{A vector of ordered levels of the factor being tested.} 
  \item{sig.level}{A vector of length 1 giving the level of significance
    of the test.} 
  \item{dfr}{A vector of length 1 giving the degrees of freedom of MSE.}
  \item{bal}{A vector of length 1 giving the information wheter the experiment
    is or not balanced.}
  \item{mt}{The model table.}
  \item{round}{A vector of length 1 to round the final result.}}

\details{
  ...}

\value{
  A list with 7 slots (\code{Table}, \code{Means}, \code{Result},
  \code{Sig.Level}, \code{Differences}, \code{MSD} (The Minimum Significative
  Difference) and \code{Replicates}) containing the most important results
  of the test performed.}

\note{
	This function is mainly for internal use in the TukeyC package.}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr}

\seealso{
  ...}

\keyword{package}
