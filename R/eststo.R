`eststo` <-
function(x,est_column){ # starting function "eststo"

summary <- summary(x)
coeff <- summary$coefficients
vars <- variable.names(x) 
col_length <- length(coeff)/4  # number of vars
est_column # output table column
dep_r_n <- c(x$call$formula[[2]],summary$r.squared,summary$adj.r,summary$df[[1]]+summary$df[[2]]) # name dep.var., R<U+00B2>, adj.R<U+00B2>, N
if(est_column != 1){coeff_col_list <<- ccl}

#coeff # control-mech
##############################################################
# reading coefficients, std.err., t-values, p-values  from the summary
# sorting it into coeff_sort_sublist and than coeff_sort_list[[i]][[j]]

for(i in  1:col_length){
        coeff_sort_sublist <- list(vars[i],coeff[i],coeff[i+col_length],coeff[i+col_length*2],coeff[i+col_length*3])
        if(i == 1){
                coeff_sort_list <- list(coeff_sort_sublist)
        }
        else{
                coeff_sort_list[i] <- list(coeff_sort_sublist)
        }
}
coeff_sort_list[col_length+1] <- list(dep_r_n)
csl <- list(coeff_sort_list)
if(est_column == 1){            # create multicolumn-table-list
        coeff_col_list <- csl
}
else{
        coeff_col_list <- c(coeff_col_list,csl)
}
return(ccl <<- coeff_col_list)
} # eststo-end

