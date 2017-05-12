setClass("methodObjSolverXY", 
         slots = c(xName = "character",
                   yName = "character"),
         contains = c("methodObj", "methodObjSolver"))

setMethod(f = ".fit",  
          signature = c(object = "methodObjSolverXY",
                        data = "data.frame",
                        response = "vector"), 
          definition = function(object, data, response, model) {

                         data <- stats::model.matrix(object = model, data = data)

                         object@methodArgs[[ object@xName ]] <- quote(data)
                         object@methodArgs[[ object@yName ]] <- quote(response)

                         fit <- try(do.call(what = object@method, 
                                            args = object@methodArgs),
                                    silent = TRUE)

                         if( is(fit, "try-error") ) {
                           df <- data.frame(data)
                           object@methodArgs[[ object@xName ]] <- quote(df)
                           object@methodArgs[[ object@yName ]] <- quote(response)

                           fit <- try(do.call(what = object@method, 
                                              args = object@methodArgs),
                                      silent = TRUE)

                           if( is(fit, "try-error") ) {
                             object@methodArgs[[ object@xName ]] <- quote(df)
                             object@methodArgs[[ object@yName ]] <- quote(data.frame(response))

                             fit <- try(do.call(what = object@method, 
                                                args = object@methodArgs),
                                        silent = TRUE)

                             if( is(fit, "try-error") ) {
                               object@methodArgs[[ object@xName ]] <- quote(data)
                               object@methodArgs[[ object@yName ]] <- quote(data.frame(response))

                               fit <- try(do.call(what = object@method, 
                                                  args = object@methodArgs),
                                          silent = TRUE)
                             }
                           }
                         }

                         if( is(fit, "try-error") ) {
                           stop("Unable to fit model.")
                         }

                         return(fit)
                       })

setGeneric(name = ".newMethodObjSolverXY", 
           def = function(method, args){
                   standardGeneric(".newMethodObjSolverXY")
                 })

setMethod(f = ".newMethodObjSolverXY",  
          signature = c(method = 'character',
                        args = 'NULL'), 
          definition = function(method, args) {

                         if( !exists(method) ) {
                           stop("solver method does not exist.")
                         }

                         args <- list("x"="x", "y"="y")

                         obj <- new("methodObjSolverXY", 
                                    xName = "x",
                                    yName = "y",
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

setMethod(f = ".newMethodObjSolverXY",  
          signature = c(method = 'character',
                        args = 'list'), 
          definition = function(method, args) {

                         if( !exists(method) ) {
                           stop("solver method does not exist.")
                         }

                         i <- which(sapply(args, function(x){all(x == "x")}))
                         if( length(i) == 0L ) {
                           args <- c("x" = "x", args)
                           xName <- "x"
                         } else {
                           xName = names(args)[i]
                         }

                         i <- which(sapply(args, function(x){all(x == "y")}))
                         if( length(i) == 0L ) {
                           args <- c("y" = "y", args)
                           yName = "y"
                         } else {
                           yName = names(args)[i]
                         }

                         obj <- new("methodObjSolverXY", 
                                    xName = xName,
                                    yName = yName,
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

setMethod(f = ".newMethodObjSolverXY",  
          signature = c(method = 'function',
                        args = 'NULL'), 
          definition = function(method, args) {

                         args <- list("x"="x", "y"="y")

                         obj <- new("methodObjSolverXY", 
                                    xName = "x",
                                    yName = "y",
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

setMethod(f = ".newMethodObjSolverXY",  
          signature = c(method = 'function',
                        args = 'list'), 
          definition = function(method, args) {

                         i <- which(sapply(args, function(x){all(x == "x")}))
                         if( length(i) == 0L ) {
                           args <- c("x" = "x", args)
                           xName = "x"
                         } else {
                           xName = names(args)[i]
                         }

                         i <- which(sapply(args, function(x){all(x == "y")}))
                         if( length(i) == 0L ) {
                           args <- c("y" = "y", args)
                           yName = "y"
                         } else {
                           yName = names(args)[i]
                         }

                         obj <- new("methodObjSolverXY", 
                                    xName = xName,
                                    yName = yName,
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

