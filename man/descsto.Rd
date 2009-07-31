\name{descsto}
\alias{descsto}
\title{Descriptive Storage}
\description{descsto stores the descriptive statistics of the summary command of data.frames and single vectors of a variable into the list object "dcl".}
\usage{descsto(x,row=NULL,name=NULL,drop.row=NULL)}

\arguments{
\item{x}{Either a data.frame of observations or a single vector.}
\item{name}{Mandatory parameter in case you add a single vector.}
\item{row}{Optional parameter. Using row= you can overwrite existing rows "dcl".}
\item{drop.row}{Vector to remove rows from the output table. Note: Braces need to be escaped with a "\\". E.g. "\\)".}
}

\value{
  The list object 'dcl' is returned.
  \item{comp1 }{variables stored}
  \item{comp2 }{variable name, 1st Qu., 2nd Qu., Median, Mean, 3rd Qu., 4th Qu., Missing Values}
  \item{comp3}{estimate, std.err., t-value, p-value}
}

\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
B <- read.table("../data/bwght.tab",header=TRUE)
attach(B)

descsto(B)
descsto(cigs,name="cigarettes")
descsto(faminc,name="faminc",row=2)
}
}
\seealso{desctab,eststo,esttab, estclear}
