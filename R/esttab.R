`esttab` <-
function(t.value=FALSE,p.value=FALSE,round.dec=3,caption=NULL,label=NULL,stars=c(0.1,0.05,0.01),filename="estout.tex",csv=FALSE){

coeff_col_list <<- ccl  # reading list from eststo
if(is.list(coeff_col_list)){  # catching use if empty ccl
}
else{return("No values stored. Program terminated!")}

var_list <- coeff_col_list[[1]][[1]][[1]] # starting list of used variables
caption <- caption
label <- label
if(csv == TRUE){
delimiter <- ","
R2 <- "R^2"
aR2 <- "adj.R^2"
N <- "N"
threestar <- "***"
twostar <- "**"
onestar <- "*"
}
else{
delimiter <- "&"
R2 <- "$R^2$"
aR2 <- "$adj.R^2$"
N <- "$N$"
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
output_matrix <- matrix(delimiter,nrow = var_count*2 + adds, ncol = length(coeff_col_list) + 1)
#output_matrix[,length(coeff_col_list) + 2] <- "\n"
for(i in 1:var_count){
        output_matrix[i*2,1] <- var_list[i]  #insert var-name in 1st column
        output_matrix[i*2+1,1] <- " "           #clean & from first column
}
#-
if(coeff_col_list[[1]][[index2]][[index3]] == "lm"){     # checking if model is of lm class
output_matrix[length(output_matrix[,1])-2,1] <- R2
output_matrix[length(output_matrix[,1])-1,1] <- aR2
output_matrix[length(output_matrix[,1]),1] <- N
}

if(coeff_col_list[[1]][[index2]][[index3]] == "plm"){     # checking if model is of plm class
output_matrix[length(output_matrix[,1]),1] <- N
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



print(output_matrix) # console print-out

if(csv == TRUE ){
write(caption,file=filename)
for (j in 1:length(output_matrix[,1])){
                write_line <- paste(output_matrix[j,],collapse="")
                write(write_line,file=filename,append=TRUE)
                }
if(t.value==TRUE){
        write("t-values in brackets",file=filename,append=TRUE)
}
else if((p.value==TRUE) && (t.value==FALSE)){
        write("p-values in brackets",file=filename,append=TRUE)
}
else{
        write("Standard errors in parentheses",file=filename,append=TRUE)
}
}
### writing tex
else{
write(paste("\\begin{table}[t]\n\\centering\n\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n\\begin{tabular}{l*{",length(output_matrix[2,])-1,"}{c}}\n\\hline\\hline",sep=""),file=filename)
for (j in 1:length(output_matrix[,1])){
                if(j == 1){
                    write(paste(paste("\t\t","&\\multicolumn{1}{c}{(",1:(length(output_matrix[1,])-1),")} ",collapse=" "),"\\\\",collapse=""),file=filename,append=TRUE)
                }
                write(paste(paste(output_matrix[j,],"\t\t",collapse=""),"\\\\",collapse=""),file=filename,append=TRUE)
                if(j == 1){
                    write("\\hline",file=filename,append=TRUE)
                }
                if(j == (length(output_matrix[,1])-3)){
                    write("\\hline",file=filename,append=TRUE)
                }
}
write("\\hline\\hline",file=filename,append=TRUE)
if(t.value==TRUE){
        write(paste("\\multicolumn{",length(output_matrix[2,]),"}{l}{\\footnotesize t-values in brackets}\\\\",sep=""),file=filename,append=TRUE)
}
else if((p.value==TRUE) && (t.value==FALSE)){
        write(paste("\\multicolumn{",length(output_matrix[2,]),"}{l}{\\footnotesize p-values in brackets}\\\\",sep=""),file=filename,append=TRUE)
}
else{
        write(paste("\\multicolumn{",length(output_matrix[2,]),"}{l}{\\footnotesize Standard errors in parentheses}\\\\",sep=""),file=filename,append=TRUE)
}
write(paste("\\multicolumn{",length(output_matrix[2,]),"}{l}{\\footnotesize \\sym{*} \\(p<",stars[1],"\\), \\sym{**} \\(p<",stars[2],"\\), \\sym{***} \\(p<",stars[3],"\\)}\\\\\n\\end{tabular}",sep=""),file=filename,append=TRUE)
write(paste("\\caption{",caption,"}",sep=""),file=filename,append=TRUE)  
write(paste("\\label{tab:",label,"}",sep=""),file=filename,append=TRUE)
write("\\end{table}",file=filename,append=TRUE)
} # end writing tex
} # esttab-end

