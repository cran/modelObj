#' @include methodObj.R
# Class \code{methodObjSolver}
#
# Extends class \code{methodObj} to indicate that the method is a regression
#   method.
#
# @name methodObjSolver-class
#
# @slot method ANY A character name or function.
# @slot methodArgs A list of inputs to be passed to the method.
#
# @keywords internal
setClass("methodObjSolver",
         contains = c("methodObj"))

# @rdname internal-fit
setMethod(f = ".fit",  
          signature = c(object = "methodObjSolver"), 
          definition = function(object, ...) {

              fit <- .fit(as(object = object, Class = "methodObj"), ...)

              if (is(object = fit, class2 = "simpleError")) {
                stop("unable to fit model", call. = FALSE)
              }

              return( fit )
            })

# Create an object of class methodObjSolver
#
# Creates an object of class methodObjSolver
#
# @param method A character name or the function for regression
# @param args A list of input arguments
#
# @return An object of class methodObjSolver
#
# @name newMethodObjSolver
# @rdname newMethodObjSolver
#
# @keywords internal
setGeneric(name = ".newMethodObjSolver", 
           def = function(args, ...){
                   standardGeneric(".newMethodObjSolver")
                 })

# @rdname modelObj-internal-api
setMethod(f = ".newMethodObjSolver",  
          signature = c(args = 'ANY'), 
          definition = function(args, method) { stop("not allowed") })

# @rdname modelObj-internal-api
setMethod(f = ".newMethodObjSolver",  
          signature = c(args = 'list'), 
          definition = function(args, method) {

              mo <- .newMethodObj(method = method, args = args)

              return( new("methodObjSolver", mo) )
            })
