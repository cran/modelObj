########################################################################
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Class modelObjXY                                                     #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# contains methods and arguments for fitting                           #
#                                                                      #
#  model : a vector of covariate names to create data matrix x         #
#                                                                      #
########################################################################
setClass("modelObjXY",
         contains = c("modelObj"))

.newModelObjXY <- function(model, 
                           solver.method,  
                           solver.args,  
                           predict.method,  
                           predict.args) {

  solver <- .newMethodObjSolverXY(method = solver.method, args = solver.args)
  predictor <- .newMethodObjPredict(method = predict.method, args = predict.args)


  obj <- new("modelObjXY", 
             model = model, 
             solver = solver,
             predictor = predictor)

  return( obj )

}
