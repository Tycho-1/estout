\name{descclear}
\alias{descclear}
\title{Cleaning up after descsto.}
\description{Cleaning dcl object created by the descsto() function.}
\usage{estclear()}

\arguments{
no further arguments necessary. Clears the transfer-object from descsto() to desctab() of the earlier models.
}

\value{descclear() clears all data stored in the ccl object that is created by the eststo() function.}
\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
descclear()
}
}
