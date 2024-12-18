% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/condition_factory.R
\name{cruleval}
\alias{cruleval}
\title{Evaluate cruler object}
\usage{
cruleval(.data, cruler = NULL, .by = NULL, usecol = TRUE)
}
\arguments{
\item{.data}{tibble}

\item{cruler}{cruler object}

\item{.by}{group by, same as \code{.by} argument in \code{dplyr::filter}}

\item{usecol}{if \code{TRUE} (default), use the default behavior of
\code{dplyr::filter()}, which allows the usage of same variable
in colnames, and filter by the data column. If \code{FALSE}, will
check whether the variables on the right side of \verb{==,>,<,>=,<=}
have the same names as columns and raise error. Ignore this
argument when using \code{.env} or \verb{!!}}
}
\value{
tibble
}
\description{
Evaluate cruler object
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
c1 <- cruler(hcpcs == "A9000")

c2 <- cruler(x > 8)

ex |> cruleval(c1)

ex |> cruleval(c1 & c2)

x <- 8
cnd <- cruler(y > x)

cruleval(ex, cnd)

try(cruleval(ex, cnd, usecol = FALSE))

cnd <- cruler(y > !!x)

cruleval(ex, cnd)

cnd <- cruler(y > .env$x)
cruleval(ex, cnd)
\dontshow{\}) # examplesIf}
}
