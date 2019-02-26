#' Hidden methods
#'
#' @name modelObj-internal-api
#' @keywords internal
#' @import methods
tmp <- function(x){}

#' Class \code{methodObj}
#'
#' Class \code{methodObj} contains a regression or prediction method and any
#'   user specified inputs to be passed to the regression or prediction method.
#'
#' @name methodObj-class
#'
#' @slot method ANY A character name or function.
#' @slot methodArgs A list of inputs to be passed to the method.
#'
#' @keywords internal
setClass("methodObj", 
         slots = c(    method = "ANY",
                   methodArgs = "list"))

#' Retrieve the method
#'
#' Returns the character or function of the regression/prediction method
#'
#' @param object An object of class \code{methodObj}
#'
#' @return A character or function.
#'
#' @name method
#' @rdname method
#'
#' @keywords internal
setGeneric(name = "method",
           def = function(object,...) { standardGeneric("method") })

#' @rdname method
setMethod(f = "method", 
          signature = c(object = "methodObj"), 
          definition = function(object,...) { return( object@method ) })

#' Retrieve the arguments to be passed to method
#'
#' Returns/Sets the list of arguments to be passed to the 
#'   regression/prediction method.
#'
#' @param object An object of class \code{methodObj}
#'
#' @return A list.
#'
#' @name methodArgs
#' @rdname methodArgs
#'
#' @keywords internal
setGeneric(name = "methodArgs",
           def = function(object,...) { standardGeneric("methodArgs") })

#' @rdname methodArgs
setMethod(f = "methodArgs", 
          signature = c(object="methodObj"), 
          definition = function(object,...) { return( object@methodArgs ) })

#' @rdname methodArgs
setGeneric(name = "methodArgs<-",
           def = function(object,value) { standardGeneric("methodArgs<-") })

#' @rdname methodArgs
setMethod(f = "methodArgs<-",   
          signature = c(object = "methodObj",
                        value = "list"), 
          definition = function(object, value) {

              nms <- names(x = object@methodArgs)
              nmsNew <- names(x = value)

              fmls <- names(x = formals(fun = object@method))
              tst <- nmsNew %in% fmls

              if (!all(tst) && !("..." %in% fmls)) {
                stop(paste(nmsNew[!tst], 
                           "not found in formal arguments of method"))
              }

              newArgs <- object@methodArgs

              for (i in 1L:length(nmsNew)) {
                newArgs[[ nmsNew[i] ]] <- value[[ i ]]
              }

              object@methodArgs <- newArgs

              return( object )
            })

#' Create an object of class methodObj
#'
#' Creates an object of class methodObj that holds the regression or prediction
#'   method and the necessary inputs.
#'
#' @param method A character providing the name of the function or the function
#'   itself.
#' @param args A list of input arguments to be passed to the function when 
#'   called.
#'
#' @return An object of class \code{methodObj}.
#'
#' @name newMethodObj
#' @rdname newMethodObj
#'
#' @keywords internal
setGeneric(name = ".newMethodObj", 
           def = function(method, args) { standardGeneric(".newMethodObj") })

#' @rdname newMethodObj
setMethod(f = ".newMethodObj",  
          signature = c(method = 'ANY',
                        args = 'ANY'), 
          definition = function(method, args) { stop("not allowed") })

#' @rdname newMethodObj
setMethod(f = ".newMethodObj",  
          signature = c(method = 'character',
                        args = 'list'), 
          definition = function(method, args) {

              if (!exists(x = method)) stop("method does not exist.")

              nms <- names(x = args)
              fmls <- names(x = formals(fun = method))

              tst <- nms %in% fmls

              if (!all(tst)) {
                if (!("..." %in% fmls)) {
                  msg <- paste(nms[!tst], 
                               "not found in formal arguments of method")
                  stop( msg )
                }
              }

              obj <- new("methodObj", 
                         method = method, 
                         methodArgs = args)

              return( obj )
            })

#' @rdname newMethodObj
setMethod(f = ".newMethodObj",  
          signature = c(method = 'function',
                        args = 'list'), 
          definition = function(method, args) {

              nms <- names(x = args)
              fmls <- names(x = formals(fun = method))

              tst <- nms %in% fmls

              if (!all(tst)) {
                if (!("..." %in% fmls)) {
                  msg <- paste(nms[!tst], 
                               "not found in formal arguments of method")
                  stop( msg )
                }
              }

              obj <- new("methodObj", 
                         method = method, 
                         methodArgs = args)

              return( obj )
            })

#' Execute Regression/Prediction Method
#'
#' Execute regression/prediction method
#'
#' @param object A methodObj object
#'
#' @return The value object as defined by the regression/prediction method 
#'   called.
#'
#' @name internal-fit
#' @rdname internal-fit
#'
#' @keywords internal
setGeneric(name = ".fit", 
           def = function(object,...) { standardGeneric(".fit") })

#' @rdname internal-fit
setMethod(f = ".fit",  
          signature = c(object = "methodObj"), 
          definition = function(object, data, response, ...) {

                mm <- tryCatch(expr = do.call(what = object@method,
                                              args = object@methodArgs),
                               error = function(e) {
                                         print(e$message)
                                         return( e )
                                       })

                return( mm )
              })

#' Obtain predictions
#'
#' Execute prediction method
#'
#' @param object A methodObjPredict object
#'
#' @return The value object as defined by the prediction method called. If a
#'   vector is returned by method it is converted to a single column matrix.
#'
#' @name internal-predict
#' @rdname internal-predict
#'
#' @keywords internal
setGeneric(name = ".predict", 
           def = function(object,...) { standardGeneric(".predict") })

#' @rdname internal-fit
setMethod(f = ".predict",  
          signature = c(object = "methodObj"), 
          definition = function(object, data, response, ...) {
                mm <- tryCatch(expr = do.call(what = object@method,
                                              args = object@methodArgs),
                               error = function(e) {
                                         print(e$message)
                                         return( e )
                                       })

                return( mm )
              })


