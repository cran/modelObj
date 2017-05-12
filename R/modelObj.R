#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# modelObj is a configuration object used to define models, methods    #
#   to be used to obtain parameter estimates, and methods to be used to#
#   obtain predictions.                                                #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# model          : An object of class formula or character vector.     #
#                  If a formula, object is model representation. If a  #
#                  character vector, covariate names to include in     #
#                  model.                                              #
#                                                                      #
# solver.method  : A character giving the R function to be used to     #
#                  obtain parameter estimates.                         #
#                  For example, `lm' or `glm'.                         #
#                                                                      #
# solver.args    : Additional arguments to be sent to solver.method.   #
#                  This must be provided as a list, where the name of  #
#                  each element matches a formal argument of           #
#                  solver.method. For example, if a logistic           #
#                  regression using glm is desired,                    #
#                     solver.method = 'glm'                            #
#                     solver.args = list(family=binomial)              #
#                                                                      #
#                  It is assumed that the solver.method takes formal   #
#                  arguments 'formula' and 'data' as inputs, such as   #
#                  lm and glm.                                         #
#                                                                      #
#                  Some R methods follow the formula/data input        #
#                  convention, but do not use formal names             #
#                  'formula' and 'data'; a user can indicate if a      #
#                  different naming convention is used for these two   #
#                  input arguments. For example, if a method expects   #
#                  the formula object to be passed through input       #
#                  variable \code{x},                                  #
#                    \code{solver.args} <- list("x"="formula")         #
#                                                                      #
#                  In addition, some R methods follow the x/y input    #
#                  convention where x is a design matrix and 'y'       # 
#                  is the response vector. This can be accomodated     #
#                  by "x" and "y" in the solver.args list.             #
#                  For example, if a method expects the design matrix  #
#                  to be passed through input argument "x",            #
#                    \code{solver.args} <- list("X"="x")               #
#                                                                      #
# predict.method : A function name giving the R function to be used to #
#                  obtain predicted values. For example, `predict.lm'  #
#                  `predict.glm'. If not explicitly given, the generic #
#                  \code{predict} is assumed. Often, this input does   #
#                  not need to be specified.                           #
#                                                                      #
# predict.args   : Additional arguments to be sent to predict.method.  #
#                  This must be provided as a list, where the name of  #
#                  each element matches a formal argument of           #
#                  predict.method. For example, if a logistic          #
#                  regression using glm was used to fit the model      #
#                  formula object,                                     #
#                     solver.method = 'glm'                            #
#                     solver.args = list(family=binomial)              #
#                  then                                                #
#                     predict.method = 'predict.glm'                   #
#                     predict.args = list(type="response")             #
#                                                                      #
#                  It is assumed that predict.method takes formal      #
#                  arguments 'object' and 'newdata' as input. Some, R  #
#                  methods are developed that do not confirm to this   #
#                  convention, such as predict.glmnet. A use can       #
#                  if a different naming convention is use for these   #
#                  two input arguments. For example, if a method       #
#                  expects the new data to be passed through input     #
#                  variable \code{newx},                               #
#                    \code{predict.args} <- list("newx"="newdata")     #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
buildModelObj <-  function(model, 
                           solver.method = NULL, 
                           solver.args = NULL, 
                           predict.method = NULL, 
                           predict.args = NULL) {

  if( is(solver.method, "NULL") ) {
    stop("must provide solver.method")
  }

  if( is(predict.method,"NULL") ) predict.method <- 'predict'

  res <- NULL

  if( !is(solver.args, "NULL") ) {

    # Check to see if user specified alternate formula input
    tstFormula <- any(sapply(solver.args, function(x){all(x == "formula")}))

    # Check to see if user specified alternate model matrix input
    tstX <- any(sapply(solver.args, function(x){all(x == "x")}))

    # Use this specification to determine if formula or XY
    if( !is.na(tstFormula) && tstFormula ) {

      res <- .newModelObjFormula(model = model,
                                 solver.method = solver.method, 
                                 solver.args = solver.args,
                                 predict.method = predict.method,
                                 predict.args = predict.args)

    } else if( !is.na(tstX) && tstX ) {

      res <- .newModelObjXY(model = model,
                            solver.method = solver.method, 
                            solver.args = solver.args,
                            predict.method = predict.method,
                            predict.args = predict.args)
    }
  }

  if( !is(res, "modelObj") ) {

    # Extract formal arguments
    inputs <- formals(solver.method)

    # Test to see if any are called formual
    tstFormula <- any(sapply(names(inputs), function(x){x == "formula"}))

    # Test to see if any are called x
    tstX <- any(sapply(names(inputs), function(x){x == "x"}))

    # Use this to determine if formula or XY
    if( tstFormula ) {
      res <- .newModelObjFormula(model = model,
                                 solver.method = solver.method, 
                                 solver.args = solver.args,
                                 predict.method = predict.method,
                                 predict.args = predict.args)
    } else if( tstX ) {

      res <- .newModelObjXY(model = model,
                            solver.method = solver.method, 
                            solver.args = solver.args,
                            predict.method = predict.method,
                            predict.args = predict.args)
    } else {
      stop("unable to determine input classes of solver.method")
    }
  }

  return(res)
}



