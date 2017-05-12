setClass("methodObjPredict", 
         slots = c(newdataName = "character",
                   objectName = "character",
                   propenMissing = "character"),
         contains = c("methodObj"))

setGeneric(name = ".newMethodObjPredict", 
           def = function(method, args){
                   standardGeneric(".newMethodObjPredict")
                 })

setGeneric(name = ".predict", 
           def = function(object,...){
                   standardGeneric(".predict")
                 })

setMethod(f = ".predict",  
          signature = c(object = "methodObjPredict"), 
          definition = function(object, newdata, fitObj) {

                         if( !missing(newdata) ) {
                           object@methodArgs[[ object@newdataName ]] <- newdata
                           object@methodArgs[[ object@objectName ]] <- fitObj

                           mm <- try(do.call(what = object@method,
                                             args = object@methodArgs),
                                     silent = TRUE)

                           if( is(mm,"try-error") ) {
                             object@methodArgs[[ object@objectName ]] <- fitObj
                             object@methodArgs[[ object@newdataName ]] <- data.matrix(newdata)

                             mm <- try(do.call(what = object@method,
                                               args = object@methodArgs),
                                       silent = TRUE)
                           }
                         } else {

                           object@methodArgs[[ object@newdataName ]] <- NULL
                           object@methodArgs[[ object@objectName ]] <- fitObj

                           mm <- try(do.call(what = object@method,
                                             args = object@methodArgs),
                                     silent = TRUE)
                         }
 
                         if( is(mm,"try-error") ) {
                           stop("prediction method could not be executed successfully")
                         }

                         if( !is(mm,"matrix") ) mm <- matrix(data = mm, ncol = 1L)

                         return(mm)
                       })

setMethod(f = ".newMethodObjPredict",  
          signature = c(method = 'character',
                        args = 'NULL'), 
          definition = function(method, args) {

                         if( !exists(method) ) {
                           stop("predict method does not exist.")
                         }

                         args <- list("object" = "object", "newdata" = "newdata")

                         obj <- new("methodObjPredict", 
                                    newdataName = "newdata",
                                    objectName = "object",
                                    propenMissing = "smallest",
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

setMethod(f = ".newMethodObjPredict",  
          signature = c(method = 'character',
                        args = 'list'), 
          definition = function(method, args) {

                         if( !exists(method) ) {
                           stop("predict method does not exist.")
                         }

                         i <- which(sapply(args, function(x){all(x == "newdata")}))
                         if( length(i) == 0L ) {
                           args <- c("newdata" = "newdata", args)
                           newdataName <- "newdata"
                         } else {
                           newdataName <- names(args)[i]
                         }

                         i <- which(sapply(args, function(x){all(x == "object")}))
                         if( length(i) == 0L ) {
                           args <- c("object" = "object", args)
                           objectName <- "object"
                         } else {
                           objectName <- names(args)[i]
                         }

                         i <- which(names(args) == "propen.missing")
                         if( length(i) == 0L ) {
                           propenMissing <- "smallest"
                         } else {
                           propenMissing <- tolower(args[[i]])
                           if( !(propenMissing %in% c("smallest","largest")) ) {
                             stop("propen.missing is inappropriate value")
                           }
                           args[[i]] <- NULL
                         }

                         obj <- new("methodObjPredict", 
                                    newdataName = newdataName,
                                    objectName = objectName,
                                    propenMissing = propenMissing,
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })


setMethod(f = ".newMethodObjPredict",  
          signature = c(method = 'function',
                        args = 'NULL'), 
          definition = function(method, args) {

                         args <- list("object" = "object", 
                                      "newdata" = "newdata")

                         obj <- new("methodObjPredict", 
                                    newdataName = "newdata",
                                    objectName = "object",
                                    propenMissing = "smallest",
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

setMethod(f = ".newMethodObjPredict",  
          signature = c(method = 'function',
                        args = 'list'), 
          definition = function(method, args) {

                         i <- which(sapply(args, function(x){all(x == "newdata")}))
                         if( length(i) == 0L ) {
                           args <- c("newdata" = "newdata", args)
                           newdataName <- "newdata"
                         } else {
                           newdataName <- names(args)[i]
                         }

                         i <- which(sapply(args, function(x){all(x == "object")}))
                         if( length(i) == 0L ) {
                           args <- c("object" = "object", args)
                           objectName <- "object"
                         } else {
                           objectName <- names(args)[i]
                         }

                         i <- which(names(args) == "propen.missing")
                         if( length(i) == 0L ) {
                           propenMissing <- "smallest"
                         } else {
                           propenMissing <- tolower(args[[i]])
                           if( !(propenMissing %in% c("smallest","largest")) ) {
                             stop("propen.missing is inappropriate value")
                           }
                           args[[i]] <- NULL
                         }

                        obj <- new("methodObjPredict", 
                                    newdataName = newdataName,
                                    objectName = objectName,
                                    propenMissing = propenMissing,
                                    method = method, 
                                    methodArgs = args)

                         return(obj)
                       })

