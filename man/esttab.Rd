\name{esttab}
\alias{esttab}
\title{Estimates Table}
\description{ Uses the data stored in the "ccl" object to create a formated table. The default is LaTeX but since version 0.5 export to CSV is possible. Therefore it is possible to import the output into a spreadsheet program and edit it for a wordprocessor. 
}
\usage{
esttab(t.value = FALSE, p.value = FALSE, round.dec = 3, caption = NULL, 
label = NULL, stars = c(0.1, 0.05, 0.01),filename="estout",csv=FALSE,
sweave=FALSE, dcolumn=NULL, table="table", table.pos="htbp",caption.top=FALSE)
}
\arguments{
\item{t.value}{if set to TRUE the table will contain t-values instead of the default standard errors}
\item{p.value}{if set to TRUE the table will contain p-values instead of the default standard errors}
\item{round.dec}{number of decimals to round to}
\item{caption}{to be used in the LaTeX output table}
\item{label}{to be used in the LaTeX output table}
\item{stars}{to change the way the stars are calculated. The values must be given as a vector from largest to smallest p-value}
\item{filename}{give an alternative filename, default is "estout.tex"}
\item{csv}{for output to csv (comma separated textfile) for direct import to a spreadsheet program. The default is TeX-output.}
\item{sweave}{if set to TRUE the esttab generates a tex-file and echoes '\\input\{filename\}' for Sweave to process.}
\item{dcolumn}{a string can be inserted that corresponds to a predefined column type in the TeX-document's head.}
\item{table}{a string for choosing a different table type like sideways or tablex.}
\item{table.pos}{for setting positioning parameters for table. For table-types that don't require these parameters insert 'NULL'.}
\item{caption.top}{if set to TRUE the caption will be inserted above the table.}
}
\seealso{ eststo, estclear }

\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
esttab(t.value=TRUE,round.dec=2,caption="Example Title",label="bwgth")
}
}
