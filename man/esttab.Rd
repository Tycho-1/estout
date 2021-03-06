\name{esttab}
\alias{esttab}
\title{Estimates Table}
\description{ Uses the data stored in the "ccl" object to create a formated table. The default is LaTeX but since version 0.5 export to CSV is possible. Therefore it is possible to import the output into a spreadsheet program and edit it for a wordprocessor. 
}
\usage{
esttab(t.value = FALSE, p.value = FALSE, round.dec = 3, 
caption = NULL,label = NULL, texfontsize = NULL, 
sig.levels = c(0.1, 0.05, 0.01), 
sig.sym=c("*","**","***"), 
filename=NULL, csv=FALSE, dcolumn=NULL, table="table", 
table.pos="htbp", caption.top=FALSE, booktabs=FALSE,
var.order=NULL, sub.sections=NULL,var.rename=NULL,
resizebox=c(0,0),colnumber=FALSE, store="default")
}
\arguments{
\item{t.value}{if set to TRUE the table will contain t-values instead of the default standard errors}
\item{p.value}{if set to TRUE the table will contain p-values instead of the default standard errors}
\item{round.dec}{number of decimals to round to}
\item{caption}{to be used in the LaTeX output table}
\item{label}{to be used in the LaTeX output table}
\item{texfontsize}{enter a LaTeX font size. (e.g. \\\\small)}
\item{sig.levels}{to change the way the stars are calculated. The values must be given as a vector from largest to smallest p-value}
\item{sig.sym}{vector of symbols to depict significance levels in TeX-tables. Insert the TeX command between the "". The vector corresponds to the sig.levels vector. Please note that due to the cat command backslash needs to be inserted twice in order to appear in the TeX document. (e.g. "\\\\alpha")}
\item{filename}{determines the filename of the output. Default is NULL, output is printed to screen.}
\item{csv}{for output to csv (comma separated textfile) for direct import to a spreadsheet program. The default is TeX-output.}
\item{dcolumn}{a string can be inserted that corresponds to a predefined column type in the TeX-document's head.}
\item{table}{a string for choosing a different table type like sideways or tablex.}
\item{table.pos}{for setting positioning parameters for table. For table-types that don't require these parameters insert 'NULL'.}
\item{caption.top}{if set to TRUE the caption will be inserted above the table.}
\item{booktabs}{if set to TRUE the \\hline commands are replaced by there corresponding booktabs commands.}
\item{var.order}{by default the order of variables is determined by there appearance in the models. Providing a vector of variables here in a different order will change the order of variables in the output table. Note that '(Intercept)' is enclosed in braces.}
\item{sub.sections}{if one needs to subdivide the table in several sections using 'subtitles' this can be done here. Providing a vector of the form c(linenumber,"subtitle",2nd linenumber,"2ndsubtitle") and so forth.}
\item{var.rename}{vector of names to replace variable abbreviations of model with real names. (e.g. var.rename=c("old.name1","new.name1","old.name2","new.name2") )}
\item{resizebox}{vector containing width and height for resizebox. (e.g. resizebox=c("\\\\textwidth","!").) Default c(0,0) disables resizebox.}
\item{colnumber}{turn on/off column / model number. Off by default.}
\item{store}{Specifies the storage unit where the data is stored that should be used for the output table.}
}
\seealso{ eststo, estclear }

\author{Felix Kaminsky \email{fkamins@uni-goettingen.de} inspired by estout for Stata.}

\examples{
\dontrun{
esttab(t.value=TRUE,round.dec=2,caption="Example Title",label="bwgth")
esttab(p.value=TRUE,colnumber=TRUE,texfontsize="\\small",store="tab2")
}
}
