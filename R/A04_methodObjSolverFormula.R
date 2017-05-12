setClass("methodObjSolverFormula", 
         slots = c(formulaName = "character",
                   dataName = "character"),
         contains = c("methodObjSolver"))

setGeneric(name = ".fit", 
           def = function(object, data, response, ...){
                   standardGeneric(".fit")
                 })

setMethod(f = ".fit",  
          signature = c(object = "methodObjSolverFormula",
                        data = "data.frame",
                        response = "vector"), 
          definition = function(object, data, response, model) {

                         #-------------------------------------------#
                         # update formula w/ response variable       #
                         #-------------------------------------------#
                         model <- stats::update(model, YinternalY ~ .)

                         #-------------------------------------------#
                         # add response to data.matrix               #
                         #-------------------------------------------#
                         nms <- colnames(data)
                         data <- cbind(data,response)
                         colnames(data) <- c(nms, "YinternalY")

                         #-------------------------------------------#
                         # Set formula argument                      #
                         #-------------------------------------------#
                         object@methodArgs[[ object@formulaName ]] <- model

                         #-------------------------------------------#
                         # Set the data argument to the local dataset#
                         #-------------------------------------------#
                         object@methodArgs[[ object@dataName ]] <- quote(data)

                         #-------------------------------------------#
                         # Perform the fit                           #
                         #-------------------------------------------#
                         fit <- try(do.call(what = object@method, 
                                            args = object@methodArgs), silent = TRUE)

                         if( is(fit, "try-error") ) {
                           stop(paste("Unable to fit model.", attr(fit,"condition")))
                         }

                         return(fit)
                       })

setGeneric(name = ".newMethodObjSolverFormula", 
           def = function(method, args){
                   standardGeneric(".newMethodObjSolverFormula")
                 })

setGeneric(name = ".newMethodObjSolverFormula", 
           def = function(method, args){
                   standardGeneric(".newMethodObjSolverFormula")
                 })

setMethod(f = ".newMethodObjSolverFormula",  
          signature = c(method = 'character',
                        args = 'NULL'), 
          definition = function(method, args) {

                         if( !exists(method) ) {
                           stop("solver method does not exist.")
                         }

                         args <- list("formula"="formula", "data"="data")

                         obj <- new("methodObjSolverFormula", 
                                    formulaName = "formula",
                                    dataName = "data",
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

setMethod(f = ".newMethodObjSolverFormula",  
          signature = c(method = 'character',
                        args = 'list'), 
          definition = function(method, args) {

                         if( !exists(method) ) {
                           stop("solver method does not exist.")
                         }

                         i <- which(sapply(args, function(x){all(x == "data")}))
                         if( length(i) == 0L ) {
                           args <- c("data" = "data", args)
                           dataName = "data"
                         } else {
                           dataName = names(args)[i]
                         }

                         i <- which(sapply(args, function(x){all(x == "formula")}))
                         if( length(i) == 0L ) {
                           args <- c("formula" = "formula", args)
                           formulaName = "formula"
                         } else {
                           formulaName = names(args)[i]
                         }

                         obj <- new("methodObjSolverFormula", 
                                    formulaName = formulaName,
                                    dataName = dataName,
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

setMethod(f = ".newMethodObjSolverFormula",  
          signature = c(method = 'function',
                        args = 'NULL'), 
          definition = function(method, args) {

                         args <- list("formula" = "formula", 
                                      "data" = "data")

                         obj <- new("methodObjSolverFormula", 
                                    formulaName = "formula",
                                    dataName = "data",
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

setMethod(f = ".newMethodObjSolverFormula",  
          signature = c(method = 'function',
                        args = 'list'), 
          definition = function(method, args) {

                         i <- which(sapply(args, function(x){all(x == "data")}))
                         if( length(i) == 0L ) {
                           args <- c("data" = "data", args)
                           dataName = "data"
                         } else {
                           dataName = names(args)[i]
                         }

                         i <- which(sapply(args, function(x){all(x == "formula")}))
                         if( length(i) == 0L ) {
                           args <- c("formula" = "formula", args)
                           formulaName = "formula"
                         } else {
                           formulaName = names(args)[i]
                         }

                         obj <- new("methodObjSolverFormula", 
                                    formulaName = formulaName,
                                    dataName = dataName,
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

