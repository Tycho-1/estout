\name{eststo}
\alias{eststo}
\title{Estimates Storage}
\description{Function that stores the estimates, R squared, adjusted R squared, number of observations, standard errors, t- and p-values in the list object "ccl".}
\usage{eststo(x,est_column)}

\arguments{
\item{x}{R object containing an estimated model.}
\item{est_column}{number of column to be used for the later table output. Can also be used to overwright an existing column with new data.}
}

\value{
  The list object 'ccl' is returned.
  \item{comp1 }{models stored}
  \item{comp2 }{estimates stored of each model, the last list contains R, adj. R, N}
  \item{comp3}{estimate, std.err., t-value, p-value}
}

\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
B <- read.table("../data/bwght.tab",header=TRUE)
attach(B)


b.cigs.motheduc <- lm(bwght ~ cigs + motheduc)
b.cigs.faminc <- lm(bwght ~ cigs + faminc)
b.cigs.motheduc.faminc <- lm(bwght ~ cigs + motheduc + faminc)



eststo(b.cigs.motheduc,1)
eststo(b.cigs.faminc,2)
eststo(b.cigs.motheduc.faminc,3)
}
}
\seealso{esttab, estclear}
