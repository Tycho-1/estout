\name{descclear}
\alias{descclear}
\title{Cleaning up after descsto.}
\description{Cleaning dcl object created by the descsto() function.}
\usage{descclear(store="default")}

\arguments{
\item{store}{Remove storage unit for specific output tables.}
}

\value{descclear() clears all data stored in the dcl object that is created by the descsto() function.}
\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
descclear()
descclear(store="tab1")
}
}
