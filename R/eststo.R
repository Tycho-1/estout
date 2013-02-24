estoutstorage <- new.env()

`eststo` <-
function(x,est_column=NULL){ # starting function "eststo"

summary <- summary(x)
class <- attributes(x)$class[[1]]
coeff <- summary$coefficients
vars <- attributes(summary$coeff)$dimnames[[1]] #variable.names(x) 
col_length <- length(coeff)/4  # number of vars

if(exists("ccl",envir=estout:::estoutstorage)){coeff_col_list <- estout:::estoutstorage$ccl}  # grabbing existing list
else{coeff_col_list <- list()}

if(is.null(est_column)){   # if no column is provided
est_column <- length(coeff_col_list)+1
}
est_column # output table column
if(length(summary$df) < 2){
        degfree <- length(attributes(summary$model)$row.names)
}
if(length(summary$df) == 3){
        degfree <- summary$df[[1]]+summary$df[[2]]
}

dep_r_n <- c(x$call$formula[[2]],summary$r.squared,summary$adj.r,degfree,class) # name dep.var., R.squared, adj.R.squared, N,model

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
assign("ccl",coeff_col_list,estout:::estoutstorage)
} # eststo-end

