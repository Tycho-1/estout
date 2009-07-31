`desctab` <- function(filename=NULL,caption=NULL,label=NULL,csv=FALSE){
###
# --- variable definition
header <- c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","Missing Values")
input_matrix <<- dcl
if(is.null(filename)){
        if(csv==TRUE){
        filename <- "descout.csv"
        }
        else{
        filename <- "descout.tex"
        }
}
# --- function


# --- csv output
if(csv == TRUE){
        write(paste(",",header,collapse=""),file=filename)
        for(i in seq(1:length(dcl))){
                write(paste(dcl[[i]],collapse=","),file=filename,append=TRUE)
        }
}
# --- Standard Output / TeX
else{
# --- writing body
        write(paste("\\begin{table}[t]\n\\centering\n\\begin{tabular}{l*{7}{c}}\n\\hline\\hline",sep=""),file=filename)
        write(paste(paste("&\t\t",header,collapse=""),"\\\\\n\\hline",collapse=""),file=filename,append=TRUE)
        for(i in seq(1:length(dcl))){
                if(length(dcl[[i]]) == 7){
                       dcl[[i]] <- append(dcl[[i]],"0")
                }
                write(paste(paste(dcl[[i]],collapse="\t\t &"),"\\\\",collapse=""),file=filename,append=TRUE)
} 
        write("\\hline\\hline\n\\end{tabular}",file=filename,append=TRUE)
        write(paste("\\caption{",caption,"}",sep=""),file=filename,append=TRUE)  
        write(paste("\\label{tab:",label,"}",sep=""),file=filename,append=TRUE)
        write("\\end{table}",file=filename,append=TRUE)
        
        }
# --- function end
}
