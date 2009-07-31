`descsto` <- function(x,row=NULL,name=NULL){
# --- grabbing parameters
input <- x
#-
# --- creating storage lists and vars
if(exists("dcl",where=1)){
        output <<- dcl
}
else{
        output <- list()
}
if(is.null(row)){   # row to be overwritten
row <- length(output)+1
}
#print(length(output))
# --- data.frame or column name
if(is(input,"data.frame")){
#print(length(input))
        for(i in seq(1:length(input))){
                column <- c(attributes(input)$names[[i]])
print(column)
                column <- c(column,summary(input[[i]]))
                output[[length(output)+1]] <- column
        }
}
else{
        if(is.null(name)){
                return('You want to insert a single column into the output. In this case it is necessary that you provide a name for the column. Please restart the function providing a name using name=!')
                #return()
        }
        else{
                output[[row]] <- c(name,summary(input))
        }
}

return(dcl <<- output)
}
