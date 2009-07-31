`esttab` <-
function(t.value=FALSE,p.value=FALSE,round.dec=3,caption=NULL,label=NULL,stars=c(0.1,0.05,0.01),filename="estout",csv=FALSE,sweave=FALSE,dcolumn=NULL,table="table",table.pos="htbp",caption.top=FALSE){

# reading list from eststo
coeff_col_list <<- ccl

# catching use if empty ccl
if(is.list(coeff_col_list)){}else{return("No values stored. I think you need to store some models first.")}

# setting dcolumn = NULL
if(is.null(dcolumn)){dcolumn <- "c"}else{dcolumn <- dcolumn}

# setting tablepos if non-NULL
if(is.null(table.pos)){}else{table.pos <- paste("[",table.pos,"]",sep="")}

# setting caption for TeX
if(is.null(caption)){texcaption <- caption}else{texcaption <- paste("\\caption{",caption,"}\n",sep="")}

# setting label if non-NULL
if(is.null(label)){}else{label <- paste("\\label{tab:",label,"}\n",sep="")}

# starting list of used variables
var_list <- coeff_col_list[[1]][[1]][[1]]

# for CSV
if(csv == TRUE){
	save.file <- paste(filename,".csv",sep="")
	delimiter <- ","
	R2 <- "R^2"
	aR2 <- "adj.R^2"
	N <- "N"
	om.end <- "\n"
	caption <- paste(caption,om.end,sep="")
	threestar <- "***"
	twostar <- "**"
	onestar <- "*"
}
# for TeX
else{
	save.file <- paste(filename,".tex",sep="")
	delimiter <- "\t\t&"
	R2 <- "$R^2$"
	aR2 <- "$adj.R^2$"
	N <- "$N$"
	om.end <- "\\\\\n"
	caption <- caption
	threestar <-  "\\sym{***}"
	twostar <-  "\\sym{**}"
	onestar <-  "\\sym{*}"
}
######### creating a vector of variable names ##########################
for (j in 1:length(coeff_col_list)){
        col_length <- length(coeff_col_list[[j]]) # length of one column list
        for (i in 1:(col_length-1)){
                var_count <- 1
               for(k in var_list){
                        if(k == coeff_col_list[[j]][[i]][[1]]){
                        break
                        }
                        else if(var_count < length(var_list)){
                                var_count <- var_count + 1
                        }
                        else if((k != coeff_col_list[[j]][[i]][[1]]) && (var_count == length(var_list))){
                                var_list <- c(var_list,coeff_col_list[[j]][[i]][[1]])
                        }
               }
        }
}
#print(var_count) #control
#print(var_list)  #control
#########################################################################

######################## making a matrix  to be filled###################
index2 <- length(coeff_col_list[[1]])  # index length of columns in model 1
index3 <- length(coeff_col_list[[1]][[index2]]) # index length of values in last columns

if(coeff_col_list[[1]][[index2]][[index3]] == "lm"){     # checking if model is of lm class
adds <- 4
}
if(coeff_col_list[[1]][[index2]][[index3]] == "plm"){     # checking if model is of plm class
adds <- 2
}

om.ncol = length(coeff_col_list) + 2	# om.ncol = number of columns of output matrix
om.nrow = var_count*2 + adds		# om.nrow = number of rows of output matrix
output_matrix <- matrix(delimiter,om.nrow,om.ncol)		# make matrix[delimiter,om.nrow,om.ncol]
output_matrix[,om.ncol] <- om.end							# fill last column
for(i in 1:var_count){
        output_matrix[i*2,1] <- var_list[i]  #insert var-name in 1st column
        output_matrix[i*2+1,1] <- " "           #clean & from first column
}
#-
if(coeff_col_list[[1]][[index2]][[index3]] == "lm"){     # checking if model is of lm class
output_matrix[length(output_matrix[,1])-2,1] <- R2
output_matrix[length(output_matrix[,1])-1,1] <- aR2
output_matrix[length(output_matrix[,1]),1] <- N
end.sep.line <- om.nrow - 3
}

if(coeff_col_list[[1]][[index2]][[index3]] == "plm"){     # checking if model is of plm class
output_matrix[length(output_matrix[,1]),1] <- N
end.sep.line <- om.nrow - 1
}
#print(output_matrix)  #control
#########################################################################
########## making stars and putting in matrix #### stars depend on p-values ##########

for (j in 1:length(coeff_col_list)){ 
        col_length <- length(coeff_col_list[[j]])
                for (i in 1:(col_length-1)){
                        if ( coeff_col_list[[j]][[i]][[5]] < stars[3] ) {
                                sigs <- paste(delimiter,round(coeff_col_list[[j]][[i]][[2]],round.dec),threestar,sep="") #coefficient
                                std_err <- paste(delimiter,"(",round(coeff_col_list[[j]][[i]][[3]],round.dec),")",sep="")        #std.err
                                t_val <- paste(delimiter,"[",round(coeff_col_list[[j]][[i]][[4]],round.dec),"]",sep="")          #t-value
                                p_val <- paste(delimiter,"[",round(coeff_col_list[[j]][[i]][[5]],round.dec),"]",sep="")          #p-value
                        }
                        else if( coeff_col_list[[j]][[i]][[5]] < stars[2] ) {
                                sigs <- paste(delimiter,round(coeff_col_list[[j]][[i]][[2]],round.dec),twostar,sep="")
                                std_err <- paste(delimiter,"(",round(coeff_col_list[[j]][[i]][[3]],round.dec),")",sep="")
                                t_val <- paste(delimiter,"[",round(coeff_col_list[[j]][[i]][[4]],round.dec),"]",sep="")
                                p_val <- paste(delimiter,"[",round(coeff_col_list[[j]][[i]][[5]],round.dec),"]",sep="")          #p-value
                        }
                        else if( coeff_col_list[[j]][[i]][[5]] < stars[1] ) {
                                sigs <- paste(delimiter,round(coeff_col_list[[j]][[i]][[2]],round.dec),onestar,sep="")
                                std_err <- paste(delimiter,"(",round(coeff_col_list[[j]][[i]][[3]],round.dec),")",sep="")
                                t_val <- paste(delimiter,"[",round(coeff_col_list[[j]][[i]][[4]],round.dec),"]",sep="")
                                p_val <- paste(delimiter,"[",round(coeff_col_list[[j]][[i]][[5]],round.dec),"]",sep="")          #p-value
                        }
                        else{
                                sigs <- paste(delimiter,round(coeff_col_list[[j]][[i]][[2]],round.dec),"",sep="")
                                std_err <- paste(delimiter,"(",round(coeff_col_list[[j]][[i]][[3]],round.dec),")",sep="")
                                t_val <- paste(delimiter,"[",round(coeff_col_list[[j]][[i]][[4]],round.dec),"]",sep="")
                                p_val <- paste(delimiter,"[",round(coeff_col_list[[j]][[i]][[5]],round.dec),"]",sep="")          #p-value
                        }
                        k <- 1
                        while(var_list[k] != coeff_col_list[[j]][[i]][[1]]){
                                k <- k +1
                        }
#print(k)
                        output_matrix[k*2,j+1] <- sigs #entry of coefficients
                        if(t.value == TRUE){
                                output_matrix[k*2+1,j+1] <- t_val  #if set entry of t-values
                        }
                        else if(p.value == TRUE){
                                output_matrix[k*2+1,j+1] <- p_val  #if set entry of t-values
                        }
                        else{
                                output_matrix[k*2+1,j+1] <- std_err  #if set entry of std.err.
                        }
                } # end for (i)
                output_matrix[1,1] <- " "
                if(csv == TRUE){
                output_matrix[1,j+1] <- paste(delimiter,coeff_col_list[[j]][[col_length]][[1]],sep="") #dep.var
                }
                else{
                output_matrix[1,j+1] <- paste('&\\multicolumn{1}{c}{',coeff_col_list[[j]][[col_length]][[1]],"}",sep="") #dep.var
                }
                if(coeff_col_list[[j]][[col_length]][[length(coeff_col_list[[j]][[col_length]])]] == "lm"){     # checking if model is of lm class
#print("Model is of class 'lm'")
                output_matrix[length(output_matrix[,j+1])-2,j+1] <- paste(delimiter,round(coeff_col_list[[j]][[col_length]][[2]],round.dec),sep="") # Rsquared
                output_matrix[length(output_matrix[,j+1])-1,j+1] <- paste(delimiter,round(coeff_col_list[[j]][[col_length]][[3]],round.dec),sep="") # adj.Rsquared
                output_matrix[length(output_matrix[,j+1]),j+1] <- paste(delimiter,round(coeff_col_list[[j]][[col_length]][[4]],round.dec),sep="") # N
                }
                if(coeff_col_list[[j]][[col_length]][[length(coeff_col_list[[j]][[col_length]])]] == "plm"){     # checking if model is of plm class
#print("Model is of class 'plm'")
                output_matrix[length(output_matrix[,j+1]),j+1] <- paste(delimiter,round(coeff_col_list[[j]][[col_length]][[2]],round.dec),sep="") # N
                }
               } # end for (j)


if(csv == TRUE ){
	sink(save.file)
	cat(caption)
	cat(output_matrix)
	if(t.value==TRUE){
	        cat("t-values in brackets")
	}
	else if((p.value==TRUE) && (t.value==FALSE)){
	        cat("p-values in brackets")
	}
	else{
	        cat("Standard errors in parentheses")
	}
	sink()
}
### writing tex
else{
# begin sink
	sink(save.file)
# collate TeX formatted table
	cat(paste("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n\\begin{",table,"}",table.pos, if(caption.top==TRUE){texcaption},"
\\centering
\\begin{tabular}{l*{",om.ncol-2,"}{",dcolumn,"}}
\\hline\\hline\n",sep=""))
	for (j in 1:om.nrow){
	                if(j == 1){
	                    cat(paste(paste("\t","&\\multicolumn{1}{c}{(",1:(om.ncol-2),")} ",collapse=" "),"\\\\",collapse=""))
	                }
	                cat(output_matrix[j,])			# writing output <- matrix
	                if(j == 1){
	                    cat("\\hline\n")
	                }
	                if(j == (end.sep.line)){
	                    cat("\\hline\n")
	                }
	}
	cat("\\hline\\hline\n")
	if(t.value==TRUE){
	        cat(paste("\\multicolumn{",om.ncol-1,"}{l}{\\footnotesize t-values in brackets}\\\\\n",sep=""))
	}
	else if((p.value==TRUE) && (t.value==FALSE)){
	        cat(paste("\\multicolumn{",om.ncol-1,"}{l}{\\footnotesize p-values in brackets}\\\\\n",sep=""))
	}
	else{
	        cat(paste("\\multicolumn{",om.ncol-1,"}{l}{\\footnotesize Standard errors in parentheses}\\\\\n",sep=""))
	}
	cat(paste("\\multicolumn{",om.ncol-1,"}{l}{\\footnotesize $^{*}$ \\(p<",stars[1],"\\), $^{**}$ \\(p<",stars[2],"\\), $^{***}$ \\(p<",stars[3],"\\)}\\\\
\\end{tabular}\n",if(caption.top==FALSE){texcaption},label,"\\end{",table,"}",sep=""))
	# end sink	
	sink()
} 
# end writing tex


# If sweave is true then only echo \input{filename}
if(sweave==TRUE){
	cat("\\input{",filename,"}\n",sep="")
}
else{
print(output_matrix) # console print-out
}
# esttab-end
} 

