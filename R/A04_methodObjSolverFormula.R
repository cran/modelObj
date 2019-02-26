#' Class \code{methodObjSolverFormula}
#'
#' Extends class \code{methodObjSolver} to indicate formula input
#'
#' @name methodObjSolverFormula-class
#'
#' @slot dataName A character giving the formal argument for input data.frame
#' @slot formulaName A character giving the formal argument for the model input
#' @slot method ANY A character name or function.
#' @slot methodArgs A list of inputs to be passed to the method.
#'
#' @keywords internal
setClass("methodObjSolverFormula", 
         slots = c(formulaName = "character",
                   dataName = "character"),
         contains = c("methodObjSolver"))

#' @rdname modelObj-internal-api
#' @importFrom stats update
setMethod(f = ".fit",  
          signature = c(object = "methodObjSolverFormula"), 
          definition = function(object, data, response, model) {

              # update formula w/ response variable
              model <- update(old = model, new = YinternalY ~ .)

              # add response to data.matrix
              nms <- colnames(x = data)
              if (is.null(x = nms)) stop("unable to obtain fit")
              data <- cbind(data, response)
              colnames(x = data) <- c(nms, "YinternalY")

              # set formula argument
              object@methodArgs[[ object@formulaName ]] <- model

              # set the data argument to the local dataset
              object@methodArgs[[ object@dataName ]] <- quote(expr = data)

              # perform the fit
              return( .fit(object = as(object = object, Class = "methodObjSolver"), 
                           data = data, response = response) )

            })

#' Create an object of class methodObjSolverFormula
#'
#' Creates an object of class methodObjSolverFormula
#'
#' @param args A list of input arguments
#'
#' @return An object of class methodObjSolverFormula
#'
#' @name newMethodObjSolverFormula
#' @rdname newMethodObjSolverFormula
#'
#' @keywords internal
setGeneric(name = ".newMethodObjSolverFormula", 
           def = function(args, ...) {
                   standardGeneric(".newMethodObjSolverFormula")
                 })

#' @rdname modelObj-internal-api
setMethod(f = ".newMethodObjSolverFormula",  
          signature = c(args = 'ANY'), 
          definition = function(args, method) { stop("not allowed") })

#' @rdname modelObj-internal-api
setMethod(f = ".newMethodObjSolverFormula",  
          signature = c(args = 'NULL'), 
          definition = function(args, method) {

              args <- list("formula" = "formula", "data" = "data")

              return( .newMethodObjSolverFormula(method = method, args = args) )
            })

#' @rdname modelObj-internal-api
setMethod(f = ".newMethodObjSolverFormula",  
          signature = c(args = 'list'), 
          definition = function(args, method) {

              i <- which(x = sapply(X = args, 
                                    FUN = function(x){ all(x == "data") }))
              if (length(x = i) == 0L) {
                args <- c("data" = "data", args)
                dataName = "data"
              } else {
                dataName = names(x = args)[i]
              }

              i <- which(x = sapply(X = args, 
                                    FUN = function(x){ all(x == "formula") }))
              if (length(x = i) == 0L) {
                args <- c("formula" = "formula", args)
                formulaName = "formula"
              } else {
                formulaName = names(x = args)[i]
              }

              mo <- .newMethodObjSolver(method = method, args = args)

              obj <- new("methodObjSolverFormula", 
                         formulaName = formulaName,
                         dataName = dataName,
                         mo)

              return( obj )
            })
