\name{estclear}
\alias{estclear}
\title{Cleaning up after eststo.}
\description{Cleaning ccl object created by the eststo() function.}
\usage{estclear()}

\arguments{
no further arguments necessary. Clears the transfer-object from eststo() to esttab() of the earlier models.
}

\value{estclear() clears all data stored in the ccl object that is created by the eststo() function.}
\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
estclear()
}
}
