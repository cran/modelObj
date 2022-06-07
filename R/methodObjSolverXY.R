#' @include methodObjSolverFormula.R
#
# Class \code{methodObjSolverXY}
#
# Extends class \code{methodObjSolver} to indicate design matrix input
#
# @name methodObjSolverXY-class
#
# @slot xName A character giving the formal argument for design input
# @slot yName A character giving the formal argument for response input
# @slot method ANY A character name or function.
# @slot methodArgs A list of inputs to be passed to the method.
#
# @keywords internal
setClass("methodObjSolverXY", 
         slots = c(xName = "character",
                   yName = "character"),
         contains = c("methodObjSolver"))

# @rdname modelObj-internal-api
#' @importFrom stats model.matrix
setMethod(f = ".fit",  
          signature = c(object = "methodObjSolverXY"), 
          definition = function(object, data, response, model) {

              data <- tryCatch(expr = stats::model.matrix(object = model, data = data),
                               error = function(e){
                                         stop("unable to obtain fit\n", 
                                              e$message, 
                                              call. = FALSE)
                                       })

              object@methodArgs[[ object@xName ]] <- as.symbol(x = "data")
              object@methodArgs[[ object@yName ]] <- as.symbol(x = "response")

              fit <- .fit(object = as(object = object, 
                                      Class = "methodObjSolver"),
                          data = data, 
                          response = response)

              if (is(object = fit, class2 = "try-error")) {

                message("attempting x as data.frame with vector y")

                fit <- .fit(object = as(object = object, 
                                        Class = "methodObjSolver"),
                            data = data.frame(data), 
                            response = response)

                if (is(object = fit, class2 = "try-error")) {

                  message("attempting x as data.frame with data.frame y")

                  fit <- .fit(object = as(object = object, 
                                          Class = "methodObjSolver"),
                              data = data.frame(data), 
                              response = data.frame(response))

                  if (is(object = fit, class2 = "try-error")) {

                    message("attempting x as data.matrix with data.frame y")

                    fit <- .fit(object = as(object = object, 
                                            Class = "methodObjSolver"),
                                data = data, 
                                response = data.frame(response))
                  }
                }
              }

              if (is(object = fit, class2 = "try-error")) {
                stop("unable to fit model", call. = FALSE)
              }

              return( fit )
            })

# Create an object of class methodObjSolverXY
#
# Creates an object of class methodObjSolverXY
#
# @param args A list of input arguments
#
# @return An object of class methodObjSolverXY
#
# @name newMethodObjSolverXY
# @rdname newMethodObjSolverXY
#
# @keywords internal
setGeneric(name = ".newMethodObjSolverXY", 
           def = function(args, ...) {
                   standardGeneric(".newMethodObjSolverXY")
                 })

# @rdname modelObj-internal-api
setMethod(f = ".newMethodObjSolverXY",  
          signature = c(args = 'ANY'), 
          definition = function(args, method) { stop("not allowed") })

# @rdname modelObj-internal-api
setMethod(f = ".newMethodObjSolverXY",  
          signature = c(args = 'NULL'), 
          definition = function(args, method) {

              args <- list("x" = "x", "y" = "y")

              return( .newMethodObjSolverXY(method = method, args = args) )
            })

# @rdname modelObj-internal-api
setMethod(f = ".newMethodObjSolverXY",  
          signature = c(args = 'list'), 
          definition = function(args, method) {

              i <- sapply(X = args, FUN = function(x){ all(x == "x") })
              if (sum(i) == 0L) {
                args <- c("x" = "x", args)
                xName <- "x"
              } else {
                xName = names(x = args)[i]
              }

              i <- sapply(X = args, FUN = function(x){ all(x == "y") })
              if (sum(i) == 0L) {
                args <- c("y" = "y", args)
                yName = "y"
              } else {
                yName = names(x = args)[i]
              }

              mo <- .newMethodObjSolver(method = method, args = args)

              obj <- new("methodObjSolverXY", 
                         xName = xName,
                         yName = yName,
                         mo)

              return( obj )
            })
