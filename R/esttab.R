`esttab` <-
function(t.value=FALSE,p.value=FALSE,round.dec=3,caption=NULL,label=NULL,stars=c(0.1,0.05,0.01)){

coeff_col_list <<- ccl
if(is.list(coeff_col_list)){
}
else{return("No values stored. Program terminated!")}

var_list <- coeff_col_list[[1]][[1]][[1]] # starting list of used variables
caption <- caption
label <- label

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
print(var_count) #control
print(var_list)  #control
#########################################################################

######################## making a matrix  to be filled###################

output_matrix <- matrix("&",nrow = var_count*2 + 4, ncol = length(coeff_col_list) + 1)
#output_matrix[,length(coeff_col_list) + 2] <- "\n"
for(i in 1:var_count){
        output_matrix[i*2,1] <- var_list[i]  #insert var-name in 1st column
        output_matrix[i*2+1,1] <- " "           #clean & from first column
}

output_matrix[length(output_matrix[,1])-2,1] <- "$R^2$"
output_matrix[length(output_matrix[,1])-1,1] <- "$adj. R^2$"
output_matrix[length(output_matrix[,1]),1] <- "$N$"
#print(output_matrix)  #control
#########################################################################
########## making stars and putting in matrix #### stars depend on p-values ##########

for (j in 1:length(coeff_col_list)){ 
        col_length <- length(coeff_col_list[[j]])
                for (i in 1:(col_length-1)){
                        if ( coeff_col_list[[j]][[i]][[5]] < stars[3] ) {
                                sigs <- paste("&",round(coeff_col_list[[j]][[i]][[2]],round.dec),"\\sym{***}",sep="") #coefficient
                                std_err <- paste("&(",round(coeff_col_list[[j]][[i]][[3]],round.dec),")",sep="")        #std.err
                                t_val <- paste("&[",round(coeff_col_list[[j]][[i]][[4]],round.dec),"]",sep="")          #t-value
                                p_val <- paste("&[",round(coeff_col_list[[j]][[i]][[5]],round.dec),"]",sep="")          #p-value
                        }
                        else if( coeff_col_list[[j]][[i]][[5]] < stars[2] ) {
                                sigs <- paste("&",round(coeff_col_list[[j]][[i]][[2]],round.dec),"\\sym{**}",sep="")
                                std_err <- paste("&(",round(coeff_col_list[[j]][[i]][[3]],round.dec),")",sep="")
                                t_val <- paste("&[",round(coeff_col_list[[j]][[i]][[4]],round.dec),"]",sep="")
                                p_val <- paste("&[",round(coeff_col_list[[j]][[i]][[5]],round.dec),"]",sep="")          #p-value
                        }
                        else if( coeff_col_list[[j]][[i]][[5]] < stars[1] ) {
                                sigs <- paste("&",round(coeff_col_list[[j]][[i]][[2]],round.dec),"\\sym{*}",sep="")
                                std_err <- paste("&(",round(coeff_col_list[[j]][[i]][[3]],round.dec),")",sep="")
                                t_val <- paste("&[",round(coeff_col_list[[j]][[i]][[4]],round.dec),"]",sep="")
                                p_val <- paste("&[",round(coeff_col_list[[j]][[i]][[5]],round.dec),"]",sep="")          #p-value
                        }
                        else{
                                sigs <- paste("&",round(coeff_col_list[[j]][[i]][[2]],round.dec),"",sep="")
                                std_err <- paste("&(",round(coeff_col_list[[j]][[i]][[3]],round.dec),")",sep="")
                                t_val <- paste("&[",round(coeff_col_list[[j]][[i]][[4]],round.dec),"]",sep="")
                                p_val <- paste("&[",round(coeff_col_list[[j]][[i]][[5]],round.dec),"]",sep="")          #p-value
                        }
                        k <- 1
                        while(var_list[k] != coeff_col_list[[j]][[i]][[1]]){
                                k <- k +1
                        }
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
                output_matrix[1,j+1] <- paste('&\\multicolumn{1}{c}{',coeff_col_list[[j]][[col_length]][[1]],"}",sep="") #dep.var
                output_matrix[length(output_matrix[,j+1])-2,j+1] <- paste("&",round(coeff_col_list[[j]][[col_length]][[2]],round.dec),sep="") # R<U+00B2>
                output_matrix[length(output_matrix[,j+1])-1,j+1] <- paste("&",round(coeff_col_list[[j]][[col_length]][[3]],round.dec),sep="") # R<U+00B2>
                output_matrix[length(output_matrix[,j+1]),j+1] <- paste("&",round(coeff_col_list[[j]][[col_length]][[4]],round.dec),sep="") # N
               } # end for (j)



print(output_matrix) # console print-out

### writing to estout.tex
write(paste("\\begin{table}[t]\n\\centering\n\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n\\begin{tabular}{l*{",length(output_matrix[2,])-1,"}{c}}\n\\hline\\hline",sep=""),file="estout.tex")
for (j in 1:length(output_matrix[,1])){
                write_line1 <- paste(output_matrix[j,],"\t\t",collapse="")
                write_line <- paste(write_line1,"\\\\",sep="")
                if(j == 1){
                    nr_write1 <- paste("\t\t","&\\multicolumn{1}{c}{(",1:(length(output_matrix[1,])-1),")} ",collapse=" ")
                    nr_write <- paste(nr_write1,"\\\\",sep="")
                    write(nr_write,file="estout.tex",append=TRUE)
                }
                write(write_line,file="estout.tex",append=TRUE)
                if(j == 1){
                    write("\\hline",file="estout.tex",append=TRUE)
                }
                if(j == (length(output_matrix[,1])-3)){
                    write("\\hline",file="estout.tex",append=TRUE)
                }
}
#write(t(output_matrix),file="estout.tex",ncolumns=(col_length+1),append=TRUE)
write("\\hline\\hline",file="estout.tex",append=TRUE)
if(t.value==TRUE){
        write(paste("\\multicolumn{",length(output_matrix[2,]),"}{l}{\\footnotesize t-values in brackets}\\\\",sep=""),file="estout.tex",append=TRUE)
}
else if((p.value==TRUE) && (t.value==FALSE)){
        write(paste("\\multicolumn{",length(output_matrix[2,]),"}{l}{\\footnotesize p-values in brackets}\\\\",sep=""),file="estout.tex",append=TRUE)
}
else{
        write(paste("\\multicolumn{",length(output_matrix[2,]),"}{l}{\\footnotesize Standard errors in parentheses}\\\\",sep=""),file="estout.tex",append=TRUE)
}
write(paste("\\multicolumn{",length(output_matrix[2,]),"}{l}{\\footnotesize \\sym{*} \\(p<",stars[1],"\\), \\sym{**} \\(p<",stars[2],"\\), \\sym{***} \\(p<",stars[3],"\\)}\\\\\n\\end{tabular}",sep=""),file="estout.tex",append=TRUE)
write(paste("\\caption{",caption,"}",sep=""),file="estout.tex",append=TRUE)  
write(paste("\\label{tab:",label,"}",sep=""),file="estout.tex",append=TRUE)
write("\\end{table}",file="estout.tex",append=TRUE)
} # esttab-end

