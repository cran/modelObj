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
setClass("modelObj",
         slots = c(    model = "formula",
                      solver = "methodObjSolver",
                   predictor = "methodObjPredict"))

setGeneric(name = ".newModelObj", 
           def = function(model, solver.method, solver.args, predict.method, predict.args){
                   standardGeneric(".newModelObj")
                 })

setMethod(f = "model", 
          signature = c(object="modelObj"), 
          definition = function(object,...){
                         return(object@model)
                       })

setMethod(f = "solver",  
          signature = c(object="modelObj"), 
          definition = function(object,...){
                         return( method(object@solver) )
                       })

setMethod(f = "solverArgs",  
          signature = c(object="modelObj"), 
          definition = function(object,...){
                         return( methodArgs(object@solver) )
                       })

setMethod(f = "solverArgs<-",   
          signature = c(object="modelObj"), 
          definition = function(object, value){
                         methodArgs(object@solver) <- value
                         return(object)
                       })

setMethod(f = "predictor",  
          signature = c(object="modelObj"), 
          definition = function(object,...){
                         return( method(object@predictor) )
                       })

setMethod(f = "predictorArgs",  
          signature = c(object="modelObj"), 
          definition = function(object,...){
                         return( methodArgs(object@predictor) )
                       })

setMethod(f = "predictorArgs<-",   
          signature = c(object="modelObj"), 
          definition = function(object, value){
                         methodArgs(object@predictor) <- value
                         return(object)
                       })


