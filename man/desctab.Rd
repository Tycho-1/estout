\name{desctab}
\alias{desctab}
\title{Description Table}
\description{ Uses the data stored in the "dcl" object to create a standard formated table. The default is LaTeX (optionally CSV) is possible. Therefore it is possible to import the output into a spreadsheet program and edit it for a wordprocessor. 
}
\usage{
desctab(filename=NULL,caption = NULL, label = NULL,csv=FALSE
, dcolumn=NULL,booktabs=FALSE,store="default")
}
\arguments{
\item{filename}{give an alternative filename, default is "descout.tex"}
\item{caption}{to be used in the LaTeX output table}
\item{label}{to be used in the LaTeX output table}
\item{csv}{for output to csv (comma separated textfile) for direct import to a spreadsheet program,default is TeX-output}
\item{dcolumn}{inserting a in the head defined dcolumn column-type.}
\item{booktabs}{if set to TRUE "\\hline" is replaced by its booktabs counterparts.}
\item{store}{Calls up specific storage unit to output this table.}
}

\seealso{ eststo, estclear, esttab, descsto }

\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
desctab(caption="Example Title",label="bwgth",filename="overview.tex")
}
}
