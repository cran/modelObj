#' @include modelObj.R methodObjSolverFormula.R methodObjPredict.R
#' @rdname modelObj
setClass("modelObjFormula",
         contains = c("modelObj"))

.newModelObjFormula <- function(model, 
                                solver.method,  
                                solver.args,  
                                predict.method,  
                                predict.args) {

  solver <- .newMethodObjSolverFormula(method = solver.method, 
                                       args = solver.args)

  predictor <- .newMethodObjPredict(method = predict.method, 
                                    args = predict.args)

  return( new("modelObjFormula", 
              model = model, 
              solver = solver,
              predictor = predictor) )
}
