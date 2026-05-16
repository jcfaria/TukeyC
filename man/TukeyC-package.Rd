\name{Tukey test}
\alias{TukeyC-package}

\docType{package}

\title{
  Conventional Tukey Test
}

\description{
  This package performs what is known as the Tukey HSD test in the conventional 
  way. It also uses an algorithm which divides the set of all means in groups 
  and assigns letters to the different groups, allowing for overlapping. This is 
  done for simple experimental designs and schemes. The most usual designs are:
  Completely Randomized Design (\samp{CRD}),
  Randomized Complete Block Design (\samp{RCBD}) and 
  Latin Squares Design (\samp{LSD}).
  The most usual schemes are:
  Factorial Experiment (\samp{FE}),
  Split-Plot Experiment (\samp{SPE}) and
  Split-Split-Plot Experiment (\samp{SSPE}).
  
  The package can be used for balanced and unbalanced experiments (when possible).

  \R has some functions
  (\code{\link[stats]{TukeyHSD}} provided by \code{stats},
  \code{\link[multcomp]{glht}} provided by \code{multcomp},
  \code{\link[agricolae]{HSD.test}} provided by \code{agricolae} and
  \code{\link[multcomp]{cld}} provided by \code{multcomp}) that also perform
  the Tukey test. The \code{TukeyHSD} returns intervals based on the range of the
  sample means rather than the individual differences. Those intervals are based 
  on Studentized range statistics and are, in essence, confidence intervals.
  This approach has two advantages: p-values are shown, allowing the user to
  allow flexible inferential decisions and make it possible to plot the 
  result of the test. However, it has one disadvantage, since the final result is 
  more difficult to understand and summarize. Others (\code{glht}, \code{cld}) 
  are also useful but difficult to manage.
  Additionally, many users of other statistical software are accustomed to 
  letters that group the means of the factor tested, which makes it unattractive or
  difficult to adapt to common \R workflows.

  So, the main aim of this package is to make available in the \R environment the
  conventional approach to the Tukey test with a set of flexible functions and
  S3 methods.
}
  
\references{
  Miller, R.G. (1981) \emph{Simultaneous Statistical Inference}. Springer.

  Ramalho M.A.P, Ferreira D.F and Oliveira A.C. (2000) \emph{Experimentacao em Genetica
  e Melhoramento de Plantas}. Editora UFLA.

  Steel, R.G., Torrie, J.H and Dickey D.A. (1997) \emph{Principles and procedures of statistics:
  a biometrical approach}. Third Edition.
  
  Yandell, B.S. (1997) \emph{Practical Data Analysis for Designed Experiments}. 
  Chapman & Hall.
}

\author{
  Faria, J. C. (\email{joseclaudio.faria@gmail.com})\cr
  Jelihovschi, E. G. (\email{eniojelihovs@gmail.com})\cr
  Allaman, I. B. (\email{ivanalaman@gmail.com})
}

\keyword{package}
