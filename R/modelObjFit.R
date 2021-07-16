#' Class \code{modelObjFit}
#'
#' A class for storing regression analysis results.
#'
#' @name modelObjFit-class
#' @rdname modelObjFit-class
#'
#' @slot fitObj Object returned by the regression analysis
#' @slot modelObj Object of class \code{modelObj}.
#'
#' @section Methods:
#'   \describe{
#'     \item{fitObject}{: Extracts regression step. }
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
#' showClass("modelObjFit")
#' 
#' @export
#' @include modelObj.R
#'
setClass("modelObjFit", 
         slots = c(fitObj = "ANY",
                   modelObj = "modelObj"))

#' Obtain parameter estimates
#'
#' Performs specified regression analysis.
#'
#' If defined by the modeling function, the following methods can be applied 
#'   to the value object returned: \code{coef}, \code{plot}, \code{predict},
#'   \code{print}, \code{residuals}, \code{show}, and \code{summary}.
#'
#' @param object An object of class \code{modelObj} as returned by the 
#'   buildModelObj function.
#' @param data An object of class data.frame containing the variables in the 
#'   model.
#' @param response An object of class vector containing the response variable.
#' @param ... ignored
#'
#' @return An object of class \code{modelObjFit}, which contains the
#'   object returned by the modeling function and the method to be used to 
#'   obtain predictions.
#'
#' @name fit
#' @usage
#'   fit(object, data, response, ...)
#'
#' @examples
#'    # generate data
#'    X <- matrix(rnorm(1000,0,1),
#'                ncol=4,
#'                dimnames=list(NULL,c("X1","X2","X3","X4")))
#'
#'    Y <- X %*% c(0.1, 0.2, 0.3, 0.4) + rnorm(250)
#'
#'    X <- data.frame(X)
#'
#'    # create modeling object using a formula
#'    mo <- buildModelObj(model=Y ~ X1 + X2 + X3 + X4,
#'                        solver.method='lm')
#'
#'    # fit model
#'    fit.obj <- fit(object=mo, data=X, response=Y)
#'
#'    coef(fit.obj)
#'    head(residuals(fit.obj))
#'    plot(fit.obj)
#'    head(predict(fit.obj,X))
#'    summary(fit.obj)
#'
#' @export
setGeneric(name = "fit", 
           def = function(object, data, response, ...) {
                   standardGeneric("fit")
                 })

#' @rdname fit
setMethod(f = "fit",
          signature = c(  object = "modelObj", 
                            data = "data.frame", 
                        response = "ANY"),
          definition = function(object, data, response, ...) {

              fitObj <- .fit(object = object@solver, 
                             data = data, 
                             response = response, 
                             model = model(object = object))

              ft <- new("modelObjFit",
                        fitObj = fitObj, 
                        modelObj = object)

              return( ft )
            })


#' Retrieve Regression Object
#'
#' Retrieves the value object returned by the regression method used to obtain 
#'   parameter estimates.
#'
#' This function is useful for accessing methods that are defined by the 
#'   regression method but are not directly accessible from the modelObjFit 
#'   object. For example, for many regression methods, users can retrieve the 
#'   fitted values by calling fitted.values(object). This method is not 
#'   directly accessible from a modelObjFit. However, fitted.values() can be 
#'   applied to the object returned by fitObject().
#'
#' @usage fitObject(object, ...)
#'
#' @param object An object of class modelObjFit.
#' @param ... ignored.
#'
#' @name fitObject
#'
#' @return The Value returned by the regression method specified in the 
#'   governing modelObj. The exact structure of the value will depend on the 
#'   regression method. For example, if nls() is the regression method, a list 
#'   is returned.
#'
#' @examples
#'    # Generate data
#'    X <- matrix(rnorm(1000,0,1),
#'                ncol=4,
#'                dimnames=list(NULL,c("X1","X2","X3","X4")))
#'
#'    Y <- X %*% c(0.1, 0.2, 0.3, 0.4) + rnorm(250)
#'
#'    X <- data.frame(X)
#'
#'    # Create modeling object using a formula
#'    mo <- buildModelObj(model=Y ~ X1 + X2 + X3 + X4,
#'                        solver.method='lm')
#'
#'    # Fit model
#'    fit.obj <- fit(object=mo, data=X, response=Y)
#'
#'    obj <- fitObject(fit.obj)
#'    fobj <- fitted.values(obj)
#'    head(fobj)
#'
#' @export
setGeneric(name = "fitObject",
           def = function(object,...) { standardGeneric("fitObject") })

#' @rdname fitObject
setMethod(f = "fitObject", 
         signature = c(object = "ANY"), 
         definition = function(object,...) { stop("not defined") })

#' @rdname fitObject
setMethod(f = "fitObject", 
         signature = c(object = "modelObjFit"), 
         definition = function(object,...) { return(object@fitObj) })

#' @rdname model
setMethod(f = "model", 
         signature = c(object = "modelObjFit"), 
         definition = function(object,...) { return( model(object@modelObj) ) })

#' @rdname predictor
setMethod(f = "predictor", 
         signature = c(object = "modelObjFit"), 
         definition = function(object,...) { return( predictor(object@modelObj) ) })

#' @rdname predictorArgs
setMethod(f = "predictorArgs", 
         signature = c(object = "modelObjFit"), 
         definition = function(object,...) { return( predictorArgs(object@modelObj) ) })

#' @rdname solver
setMethod(f = "solver", 
         signature = c(object = "modelObjFit"), 
         definition = function(object,...) { return( solver(object@modelObj) ) })

#' @rdname solverArgs
setMethod(f = "solverArgs", 
         signature = c(object = "modelObjFit"), 
         definition = function(object,...) { return( solverArgs(object@modelObj) ) })

#' @describeIn modelObjFit Extract Model Coefficients
#' @importFrom stats coef
#' @export
setMethod(f = "coef",
          signature = c(object = "modelObjFit"),
          definition = function(object,...) {

              tmp <- tryCatch(expr = coef(object = object@fitObj,...), 
                              error = function(e){
                                        warnMsg(x = "coef", 
                                                cx = class(x = object@fitObj))
                                        return( NULL )
                                      })
              return( tmp )
            })

#' @describeIn modelObjFit X-Y plotting
#' @param x An object of class modelObjFit
#' @param y ignored
#' @export
setMethod(f = "plot",
          signature = c(x = "modelObjFit"),
          definition = function(x, ...){
              tmp <- tryCatch(expr = plot(x = x@fitObj, ...), 
                              error = function(e){
                                        warnMsg(x = "plot", 
                                                cx = class(x = x@fitObj))
                                      })
            })

#' @describeIn modelObjFit Print regression results
#' @param x An object of class modelObjFit
#' @export
setMethod(f = "print",
          signature = c(x = "modelObjFit"),
          definition = function(x) { print(x@fitObj) })

#' Model Predictions
#'
#' Predictions from the results of a fit object.
#'
#' @param object An object of class \code{modelObjFit} as returned by the 
#'   fit() function.
#' @param newdata An object of class data.frame containing the variables in the 
#'   model.
#' @param ... ignored
#'
#' @return Model predictions, the form of which depend on the regression analysis.
#'
#' @name predict
#' @rdname predict
#' @usage
#'   predict(object, ...)
#'
#' @examples
#'    # generate data
#'    X <- matrix(rnorm(1000,0,1),
#'                ncol=4,
#'                dimnames=list(NULL,c("X1","X2","X3","X4")))
#'
#'    Y <- X %*% c(0.1, 0.2, 0.3, 0.4) + rnorm(250)
#'
#'    X <- data.frame(X)
#'
#'    # create modeling object using a formula
#'    mo <- buildModelObj(model=Y ~ X1 + X2 + X3 + X4,
#'                   solver.method='lm')
#'
#'    # fit model
#'    fit.obj <- fit(object=mo, data=X, response=Y)
#'
#'    predict(fit.obj)
#'    predict(fit.obj, newdata = X[1:10,])
#'
#' @importFrom stats predict
#' @export
NULL

#' @rdname predict
setMethod(f = "predict",
          signature = c(object="modelObjFit"),
          definition = function(object, newdata, ...) {

              if (missing(x = newdata)) {

                res <- .predict(object = object@modelObj@predictor,
                                fitObj = object@fitObj)

              } else {

                res <- .predict(object = object@modelObj@predictor,
                                newdata = newdata,
                                fitObj = object@fitObj,
                                model = object@modelObj@model)

              }

              return( res )
            })
                       
#' @describeIn modelObjFit Extract residuals
#' @param object An object of class modelObjFit
#' @export
setMethod(f = "residuals",
          signature = c(object = "modelObjFit"),
          definition = function(object, ...) {

              tmp <- tryCatch(expr = residuals(object = object@fitObj,...), 
                              error = function(e){
                                        warnMsg(x = "residuals", 
                                                cx = class(x = object@fitObj))
                                        return( NULL )
                                      })

              if (!is(object = tmp, class2 = "matrix")) {
                tmp <- matrix(data = tmp, 
                              ncol = 1L, 
                              dimnames=list(NULL, "residuals"))
              }
              return( tmp )
             })

#' @describeIn modelObjFit Show regression results
#' @param object An object of class modelObjFit
#' @export
setMethod(f = "show",
          signature = c(object="modelObjFit"),
          definition = function(object) { show(object = object@fitObj) })

#' @describeIn modelObjFit Show summary results
#' @param object An object of class modelObjFit
#' @param ... passed to underlying method defined for regression value object.
#' @export
setMethod(f = "summary",
          signature = c(object="modelObjFit"),
          definition = function(object,...){
              tmp <- tryCatch(expr = summary(object = object@fitObj,...), 
                              error = function(e){
                                        warnMsg(x = "summary", 
                                                cx = class(x = object@fitObj))
                                        return( NULL )
                                      })
              return( tmp )
             })
