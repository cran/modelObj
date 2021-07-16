#' @include modelObj.R methodObjSolverXY.R methodObjPredict.R
#' @rdname modelObj
setClass("modelObjXY",
         contains = c("modelObj"))

.newModelObjXY <- function(model, 
                           solver.method,  
                           solver.args,  
                           predict.method,  
                           predict.args) {

  solver <- .newMethodObjSolverXY(method = solver.method, 
                                  args = solver.args)
  predictor <- .newMethodObjPredict(method = predict.method, 
                                    args = predict.args)

  return( new("modelObjXY", 
              model = model, 
              solver = solver,
              predictor = predictor) )
}
