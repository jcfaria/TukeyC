\name{make.TukeyC.test}
\alias{make.TukeyC.test}

\title{
  Make Tukey Test
}

\description{
  Performs all pairwise Tukey comparisons for balanced or unbalanced designs and
  assembles the result table with letter groups.
}

\usage{
make.TukeyC.test(obj,
                 MSE,
                 sig.level,
                 dfr,
                 round,
                 adjusted.pvalue)
}

\arguments{
  \item{obj}{A \code{data.frame} with columns \code{means} and \code{reps} and
    row names giving the factor levels.}
  \item{MSE}{Mean squared error (single numeric value).}
  \item{sig.level}{Significance level for the test (e.g. \code{0.05}).}
  \item{dfr}{Residual degrees of freedom associated with \code{MSE}.}
  \item{round}{Number of decimal places for formatted means.}
  \item{adjusted.pvalue}{Method passed to \code{\link{p.adjust}} for the
    pairwise \code{p}-values (e.g. \code{"none"}).}
}

\value{
  A list with:
  \describe{
    \item{Result}{\code{data.frame} with column \code{Means} and letter columns
      \code{G1}, \code{G2}, \ldots}
    \item{Sig.level}{Significance level used.}
    \item{Diff_Prob}{Matrix of mean differences (upper triangle) and adjusted
      \code{p}-values (lower triangle).}
    \item{MSD}{Minimum significant differences for each pair.}
    \item{Replicates}{Replicate counts per level.}
  }
}

\note{
  This function is for internal use in the \pkg{TukeyC} package.
}

\author{
  Faria, J. C. (\email{joseclaudio.faria@gmail.com})\cr
  Jelihovschi, E. G. (\email{eniojelihovs@gmail.com})\cr
  Allaman, I. B. (\email{ivanalaman@gmail.com})
}

\keyword{internal}
