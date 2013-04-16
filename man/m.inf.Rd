\name{m.inf}
\alias{m.inf}
\alias{m.inf.1a}
\alias{m.inf.1b}
\alias{m.inf.2a}
\alias{m.inf.2b}
\alias{m.inf.3a}
\alias{m.inf.3b}

\title{
  Calculates the Means and Dispersion
}

\description{
  Calculates the means and dispersion for one, two and three factor(s) by \code{model} and \code{model.frame.aovlist} functions.
}

\usage{
  m.inf.1a(x,
           which,
           dispersion=c('mm', 's', 'se'))
  m.inf.1b(x,
           which,
           dispersion=c('mm', 's', 'se'))
  m.inf.2a(x,
           which1,
           which2,
           dispersion=c('mm', 's', 'se'))
  m.inf.2b(x,
           which1,
           which2,
           dispersion=c('mm', 's', 'se'))
  m.inf.3a(x,
           which1,
           which2,
           which3,
           dispersion=c('mm', 's', 'se'))
  m.inf.3b(x,
           which1,
           which2,
           which3,
           dispersion=c('mm', 's', 'se'))
}

\arguments{
  \item{x}{A \code{SK} object.}
  \item{which}{The name of the treatment to be used in the comparison.
    For all the value is determined internally by the package.}
  \item{which1}{The name of the treatment to be used in the comparison.
    For all the value is determined internally by the package.}
  \item{which2}{The name of the treatment to be used in the comparison.
    For all the value is determined internally by the package.}
  \item{which3}{The name of the treatment to be used in the comparison.
    For all the value is determined internally by the package.}
  \item{dispersion}{The dispersion to be considered to the means. \cr
    The possible vaues are: \samp{'mm'} = \samp{minimum and maximum},
    \samp{'s'} = \samp{standart deviation},
    \samp{'se'} = \samp{standart deviation of the mean}.}
}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr
}

\note{
  This function is mainly for internal use in the \pkg{ScottKnott} package.
}

\keyword{m.inf}

