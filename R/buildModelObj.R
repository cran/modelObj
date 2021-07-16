#' Create an Object of Class modelObj
#'
#' A utility function to transfer user defined models and estimation methods 
#'   to an object of class modelObj.
#'
#' Unless changed by the user in solver.args and/or predict.args, default 
#'   settings are assumed for the specified regression and prediction methods.  
#'
#' @name buildModelObj
#'
#' @param model An object of class formula; the model.
#'  
#' @param solver.method An object of class character specifying the name of  
#'   the R function to be used to obtain parameter estimates. Or, the function  
#'   to be used to obtain parameter estimates. For example,  `lm', `glm',  
#'   or `rpart'. The specified modeling function MUST have a corresponding  
#'   predict method.
#'  
#' @param solver.args An object of class list containing additional arguments 
#'   to be sent to solver.method.  Arguments must be provided as a list, where  
#'   the name of each element matches a formal argument of solver.method. For  
#'   example, if a logistic regression using glm is desired, 
#'   \deqn{solver.method = ``glm"}
#'   \deqn{solver.args = list(``family"=binomial)}
#' 
#'   A solver.method can takes formal arguments 'formula' and 'data' as inputs, 
#'   such as lm and glm. Some R methods do not use formal names 'formula' and 
#'   'data'; a user can indicate if a different naming convention is used for 
#'   these two input arguments. For example, if a method expects the formula 
#'   object to be passed through input variable \code{x},  
#'   \code{solver.args} <- list("x"="formula")
#' 
#'   A solver.method can also take formal arguments 'x' and 'y' as inputs,  
#'   such as glmnet. Some R methods do not use formal names 'x' and 'y' to  
#'   indicate the covariate and response; a user can indicate if a different 
#'   naming convention is used for these two input arguments. For example, if a
#'   method expects the covariate matrix to be passed through input variable  
#'   \code{X}, \code{solver.args} <- list("X"="x")
#'  
#' @param predict.method A character. The name of the R function or the  
#'   function to be used to obtain predictions. For example,  `predict.lm',  
#'   `predict', or `predict.glm'. If no function is explicitly given, the  
#'   generic \code{predict} is assumed. For many methods, the generic method  
#'   is appropriate.
#'  
#' @param predict.args A list. Additional arguments to be sent to  
#'   predict.method. This must be provided as a list, where the name of each  
#'   element matches a formal argument of predict.method. For example, if a  
#'   logistic regression using glm was used to fit the model formula object  
#'   and predictions on the scale of the response are desired, 
#'    \deqn{predict.method = ``predict.glm"}
#'    \deqn{predict.args = list(``type"=``response").}
#' 
#'   It is assumed that the predict.method has formal arguments ``object"  
#'   and ``newdata". If predict.method does not use these formal arguments,  
#'   predict.args must explicitly indicate the variable names used for these  
#'   inputs. For example, list(``newx"=``newdata") if the new data is passed  
#'   to predict.method through input argument ``newx".
#'
#' @return An object of class \code{modelObjFormula} or \code{modelObjXY}, which
#'   inherit directly from \link{modelObj}.
#'
#' @export
#' @examples
#'    #----------------------------------------------------#
#'    # Create modeling object using a formula
#'    #----------------------------------------------------#
#'    mo <- buildModelObj(model=Y ~ X1 + X2 + X3 + X4,
#'                        solver.method='lm', 
#'                        predict.method='predict.lm',
#'                        predict.args=list(type='response'))
#' @include modelObjXY.R modelObjFormula.R
#'

buildModelObj <-  function(model, 
                           solver.method = NULL, 
                           solver.args = NULL, 
                           predict.method = NULL, 
                           predict.args = NULL) {

  if (is(object = solver.method, class2 = "NULL")) {
    stop("must provide solver.method")
  }

  if (is(object = predict.method, class2 = "NULL")) predict.method <- 'predict'

  res <- NULL

  if (!is(object = solver.args, class2 = "NULL")) {

    # check to see if user specified alternate formula input
    tstFormula <- any(sapply(X = solver.args, 
                             FUN = function(x){ all(x == "formula") }))

    # check to see if user specified alternate model matrix input
    tstX <- any(sapply(X = solver.args, 
                       FUN = function(x){ all(x == "x") }))

    # use this specification to determine if formula or XY
    if (!is.na(x = tstFormula) && tstFormula) {

      res <- .newModelObjFormula(model = model,
                                 solver.method = solver.method, 
                                 solver.args = solver.args,
                                 predict.method = predict.method,
                                 predict.args = predict.args)

    } else if (!is.na(x = tstX) && tstX) {

      res <- .newModelObjXY(model = model,
                            solver.method = solver.method, 
                            solver.args = solver.args,
                            predict.method = predict.method,
                            predict.args = predict.args)
    }
  }

  if (!is(object = res, class2 = "modelObj")) {

    # extract formal arguments
    inputs <- formals(solver.method)

    # test to see if any are called formula
    tstFormula <- any(sapply(X = names(x = inputs), 
                             FUN = function(x){ x == "formula" }))

    # test to see if any are called x
    tstX <- any(sapply(X = names(x = inputs), 
                       FUN = function(x){ x == "x" }))

    # use this to determine if formula or XY
    if (tstFormula) {
      res <- .newModelObjFormula(model = model,
                                 solver.method = solver.method, 
                                 solver.args = solver.args,
                                 predict.method = predict.method,
                                 predict.args = predict.args)
    } else if (tstX) {

      res <- .newModelObjXY(model = model,
                            solver.method = solver.method, 
                            solver.args = solver.args,
                            predict.method = predict.method,
                            predict.args = predict.args)
    } else {
      stop("unable to determine input classes of solver.method")
    }
  }

  return( res )
}
