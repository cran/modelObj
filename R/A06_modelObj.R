#' Class \code{modelObj}
#'
#' A class for model objects.
#'
#' Objects should not be created directly. The utility function
#' buildModelObj() should be used.
#'
#' @name modelObj
#' @rdname modelObj
#'
#' @slot model Object of class \code{formula}
#' @slot solver Object of class \code{methodObjSolver} method to obtain 
#'   parameter estimates.
#' @slot predictor Object of class \code{methodObjPredict} method to obtain 
#'   predicted values.
#'
#' @section Methods:
#'   \describe{
#'     \item{fit}{: Executes regression step. }
#'     \item{model}{: Retrieve model. }
#'     \item{solver}{: Retrieve regression method name. }
#'     \item{solverArgs}{: Retrieve arguments to be sent to regression method. }
#'     \item{solverArgs(object)<-}{: Set arguments to be sent to regression method. }
#'     \item{predictor}{: Retrieve prediction method name. }
#'     \item{predictorArgs}{: Retrieve arguments to be sent to prediction method. }
#'     \item{predictorArgs(object)<-}{: Set arguments to be sent to prediction method. }
#'   }
#' 
#' @examples
#' showClass("modelObj")
#' 
setClass(Class = "modelObj",
         slots = c(    model = "formula",
                      solver = "methodObjSolver",
                   predictor = "methodObjPredict"))

#' Retrieve model
#'
#' Retrieves model from modelObj
#'
#' @param object A modelObj object
#' @param ... ignored
#'
#' @return The formula for the regression
#'
#' @name model
#' @rdname model
#'
#' @export
setGeneric(name = "model",
           def = function(object,...) { standardGeneric("model") })

#' @rdname modelObj-internal-api
setMethod(f = "model", 
          signature = c(object="modelObj"), 
          definition = function(object,...) { return(object@model) })

#' Retrieve Solver Method
#'
#' Retrieves method for regression analysis
#'
#' @param object A modelObj object
#' @param ... ignored
#'
#' @return An object of class character or function
#'
#' @name solver
#' @rdname solver
#'
#' @export
setGeneric(name = "solver",
           def = function(object,...) { standardGeneric("solver") })

#' @rdname modelObj-internal-api
setMethod(f = "solver",  
          signature = c(object="modelObj"), 
          definition = function(object,...) { return( method(object@solver) ) })

#' Retrieve Solver Arguments
#'
#' Retrieves the arguments that are to be passed to the regression method
#'   when called.
#'
#' @param object A modelObj object
#' @param ... ignored
#'
#' @return A list
#'
#' @name solverArgs
#' @rdname solverArgs
#'
#' @export
setGeneric(name = "solverArgs",
           def = function(object,...) { standardGeneric("solverArgs") })

#' @rdname modelObj-internal-api
setMethod(f = "solverArgs",  
          signature = c(object="modelObj"), 
          definition = function(object,...) {
              return( methodArgs(object@solver) )
            })

#' @rdname solverArgs
#' @param value List to be stored in args
#'
#' @export
setGeneric(name = "solverArgs<-",
           def = function(object,value) { standardGeneric("solverArgs<-") })

#' @rdname modelObj-internal-api
setMethod(f = "solverArgs<-",   
          signature = c(object="modelObj"), 
          definition = function(object, value) {
              methodArgs(object@solver) <- value
              return( object )
            })

#' Retrieve Prediction Method
#'
#' Retrieves method for prediction analysis
#'
#' @param object A modelObj object
#' @param ... ignored
#'
#' @return An object of class character or function
#'
#' @name predictor
#' @rdname predictor
#'
#' @export
setGeneric(name = "predictor",
           def = function(object,...) { standardGeneric("predictor") })

#' @rdname modelObj-internal-api
setMethod(f = "predictor",  
          signature = c(object="modelObj"), 
          definition = function(object,...) {
              return( method(object@predictor) )
            })

#' Retrieve Predictor Arguments
#'
#' Retrieves the arguments that are to be passed to the prediction method
#'   when called.
#'
#' @param object A modelObj object
#' @param ... ignored
#'
#' @return A list
#'
#' @name predictorArgs
#' @rdname predictorArgs
#'
#' @export
setGeneric(name = "predictorArgs",
           def = function(object,...) { standardGeneric("predictorArgs") })

#' @rdname modelObj-internal-api
setMethod(f = "predictorArgs",  
          signature = c(object="modelObj"), 
          definition = function(object,...) {
              return( methodArgs(object@predictor) )
            })

#' @rdname predictorArgs
#' @param value List to be stored in args
#'
#' @export
setGeneric(name = "predictorArgs<-",
           def = function(object, value) { standardGeneric("predictorArgs<-") })

#' @rdname modelObj-internal-api
setMethod(f = "predictorArgs<-",   
          signature = c(object="modelObj"), 
          definition = function(object, value) {
              methodArgs(object@predictor) <- value
              return( object )
            })
