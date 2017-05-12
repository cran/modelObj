########################################################################
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Class modelObjFormula                                                #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# contains methods and arguments for fitting                           #
#                                                                      #
#  model : a single formula objects to be evaluated                    #
#                                                                      #
#  solver : methodObj for parameter estimates                          #
#                                                                      #
#  predictor : methodObj for predictions                               #
########################################################################
setClass("modelObjFormula",
         contains = c("modelObj"))

.newModelObjFormula <- function(model, 
                                solver.method,  
                                solver.args,  
                                predict.method,  
                                predict.args) {

  solver <- .newMethodObjSolverFormula(method = solver.method, args = solver.args)
  predictor <- .newMethodObjPredict(method = predict.method, args = predict.args)

  obj <- new("modelObjFormula", 
             model = model, 
             solver = solver,
             predictor = predictor)

  return( obj )

}
