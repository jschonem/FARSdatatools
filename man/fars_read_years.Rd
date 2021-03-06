% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Extract Month Data}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A value or vector/list of values indicating the year(s) of the
data desired. Values of class integer or numeric may be passed safely to
this function. Values of class character are also safe if coercible to
integer, e.g. '2013'. Data only exist for the years 2013, 2014, & 2015. An
error will be generated if a year other than one of these is passed to the
function.}
}
\value{
This function returns a list of data.frame objects. The members of
the returned list contain the month data corresponding to the year
value(s) passed to the function.
}
\description{
This function extracts month data from a file of accident records for
specified years. These data are the months in which the accidents recorded
in the data file occurred.
}
\examples{
\dontrun{fars_read_years(2015)
fars_read_years(c('2013','2014'))
fars_read_years(list(2014,2015)) }

}
