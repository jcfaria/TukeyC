\name{make.TukeyC.groups}
\alias{make.TukeyC.groups}

\title{
  Make Tukey Groups
}

\description{
  Builds compact letter groups from the full pairwise significance matrix
  produced by the Tukey test.
}

\usage{
make.TukeyC.groups(x)
}

\arguments{
  \item{x}{A square logical matrix whose rows and columns are factor levels
    sorted in decreasing order of the means. \code{FALSE} indicates that the
    pair is \emph{not} significantly different; \code{TRUE} indicates a
    significant difference.}
}

\details{
  The algorithm scans each treatment, forms candidate groups of non-significant
  comparisons, enforces internal consistency when pairwise patterns are
  non-transitive (as in severely unbalanced designs), removes redundant
  subsets, and writes the familiar column-wise letter display (\code{G1},
  \code{G2}, \ldots).
}

\value{
  A character matrix with row names equal to the treatment levels (sorted by
  decreasing mean) and one column per letter group (\code{G1}, \code{G2},
  \ldots). Empty cells indicate that the treatment does not carry that letter.
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
