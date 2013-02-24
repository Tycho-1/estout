\name{estclear}
\alias{estclear}
\title{Cleaning up after eststo.}
\description{Cleaning ccl object created by the eststo() function.}
\usage{estclear(store="default")}

\arguments{
When using the default storage unit, no parameters are necessary.
\item{store}{Parameter to clear specific table storage unit.}
}

\value{estclear() clears all data stored in the ccl object that is created by the eststo() function.}
\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
estclear()
estclear(store="tab1")
}
}
