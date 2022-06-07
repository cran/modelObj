#' @include methodObj.R
#
# Class \code{methodObjPredict}
#
# Extends class \code{methodObj} to indicate that the method is a prediction
#   method.
#
# @name methodObjPredict-class
#
# @slot newdataName A character giving the formal argument for input data.frame
# @slot objectName A character giving the formal argument for the input 
#   regression object
# @slot propenMissing A character indicating if a treatment variable is missing
# @slot method ANY A character name or function.
# @slot methodArgs A list of inputs to be passed to the method.
#
# @keywords internal
setClass("methodObjPredict", 
         slots = c(newdataName = "character",
                   objectName = "character",
                   propenMissing = "character"),
         contains = c("methodObj"))

# Create an object of class methodObjPredict
#
# Creates an object of class methodObjPredict
#
# @param method A character name or the function for making predictions
# @param args A list of input arguments
#
# @return An object of class methodObjPredict
#
# @name newMethodObjPredict
# @rdname newMethodObjPredict
#
# @keywords internal
setGeneric(name = ".newMethodObjPredict", 
           def = function(args, ...) {
                   standardGeneric(".newMethodObjPredict")
                 })

# @rdname modelObj-internal-api
setMethod(f = ".newMethodObjPredict",  
          signature = c(args = 'ANY'), 
          definition = function(args, method) { stop("not allowed") })

# @rdname modelObj-internal-api
setMethod(f = ".newMethodObjPredict",  
          signature = c(args = 'NULL'), 
          definition = function(args, method) {

              args <- list("object" = "object", 
                           "newdata" = "newdata",
                           "propenMissing" = "smallest")

              return( .newMethodObjPredict(method = method, args = args) )
            })

# @rdname modelObj-internal-api
setMethod(f = ".newMethodObjPredict",  
          signature = c(args = 'list'), 
          definition = function(args, method) {

              i <- sapply(X = args, FUN = function(x){all(x == "newdata")})
              if (sum(i) == 0L) {
                args <- c("newdata" = "newdata", args)
                newdataName <- "newdata"
              } else {
                newdataName <- names(x = args)[i]
              }

              i <- sapply(X = args, FUN = function(x){all(x == "object")})
              if (sum(i) == 0L) {
                args <- c("object" = "object", args)
                objectName <- "object"
              } else {
                objectName <- names(x = args)[i]
              }

              i <- which(x = names(x = args) == "propen.missing")
              if (length(x = i) == 0L) {
                propenMissing <- "smallest"
              } else {
                propenMissing <- tolower(x = args[[ i ]])
                if (!{propenMissing %in% c("smallest","largest")}) {
                  stop("propen.missing is inappropriate value")
                }
                args[[ i ]] <- NULL
              }

              mo <- .newMethodObj(method = method, args = args)

              obj <- new("methodObjPredict", 
                         newdataName = newdataName,
                         objectName = objectName,
                         propenMissing = propenMissing,
                         mo)

              return( obj )
            })

# @rdname internal-predict
# @param newdata A data.frame of model covariates
# @param fitObj The value object returned by the regression
# @importFrom stats model.matrix
setMethod(f = ".predict",  
          signature = c(object = "methodObjPredict"), 
          definition = function(object, newdata, fitObj, model) {

              if (!missing(x = newdata)) {

                object@methodArgs[[ object@newdataName ]] <- as.symbol("newdata")
                object@methodArgs[[ object@objectName ]] <- as.symbol("fitObj")

                mm <- .predict(object = as(object = object, Class = "methodObj"),
                               newdata = newdata, fitObj = fitObj)

                if (is(object = mm, class2 = "simpleError")) {

                  message("converting newdata to data.matrix and trying again")

                  mm <- .predict(object = as(object = object, 
                                             Class = "methodObj"),
                                 newdata = data.matrix(frame = newdata), 
                                 fitObj = fitObj)

                  if (is(object = mm, class2 = "simpleError")) {
                    message("converting newdata to model.matrix and trying again")

                    mm <- .predict(object = as(object = object, 
                                               Class = "methodObj"),
                                   newdata = stats::model.matrix(object = model, 
                                                                 data = newdata), 
                                   fitObj = fitObj)
                  }

                }
              } else {

                object@methodArgs[[ object@newdataName ]] <- NULL
                object@methodArgs[[ object@objectName ]] <- fitObj

                mm <- .predict(object = as(object = object, Class = "methodObj"))
              }

              if (is(object = mm, class2 = "simpleError")) {
                stop("prediction method could not be executed successfully",
                     call. = FALSE)
              }

              if (!is(object = mm, class2 = "matrix")) {
                mm <- matrix(data = mm, ncol = 1L)
              }

              return( mm )
            })
