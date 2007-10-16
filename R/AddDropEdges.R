
## ## #################################################################
## ##
## ## Add and drop edges
## ##
## ## #################################################################

#dropEdge <- function(object, name.1, name.2) UseMethod("dropEdge")
dropEdge.gModel <- 
          function(object,name.1,name.2) {
            
            ## cat("Drop:",name.1,name.2,"\n",sep=" ")              
            ## edit hllm formula
            form <- formula(object)
            listform <- readf(form[2])
            new.form <- delete.edge(listform,c(name.1,name.2))
              
            form <- paste("~",showf(new.form))
            formula(object) <- as.formula(form)

            if (inherits(object,"gRfit"))
              object <- fit(object)

            return(object)
          }


#addEdge <- function(object, name.1, name.2) UseMethod("addEdge")
addEdge.gModel <- 
          function(object,name.1,name.2) {
            
            new.object <- object
            ## edit hllm formula
            form <- formula(object)
            listform <- readf(form[2])
            new.form <- add.edge(listform,c(name.1,name.2))
            form <- paste("~",showf(new.form))
            formula(new.object) <- as.formula(form)

            if (inherits(new.object,"gRfit"))
              new.object <- fit(new.object)
            
            return(new.object)
          }
















## dropVertex <- function(object, name) UseMethod("dropVertex")
## dropVertex.gModel <- 
##   function(object,name) {
##             ## edit hllm formula
##             form <- formula(object)
##             listform <- readf(form[2])

##             ## delete 'name' from generators
##             new.form <- lapply(listform,setdiff,name)
##             form <- paste("~",showf(new.form))
##             formula(object) <- as.formula(form)
            
##             if (inherits(object,"gRfit"))
##               object <- fit(object)
            
##             return(object)
##           }


## addVertex <- function(object, name) UseMethod("addVertex")
## addVertex.gModel <- 
##           function(object,name) {
##             ## edit formula
##             form <- formula(object)
##             listform <- readf(form[2])
##             listform[[length(listform)+1]] <- name
##             form <- paste("~",showf(listform))
##             formula(object) <- as.formula(form)
##             if (inherits(object,"gRfit"))
##               object <- fit(object)
            
##             return(object)
##             }
