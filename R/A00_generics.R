#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                      #
# Obtains parameter estimates for formula objects                      #
# Returns an object of class modelObjFit                               #
#                                                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
setGeneric(name = "fit", 
           def = function(object, data, response, ...){
                   standardGeneric("fit")
                 })

#----------------------------------------------------------------------#
# Method to retrieve value object returned by fit method.              #
#----------------------------------------------------------------------#
setGeneric(name = "fitObject",
           def = function(object,...){
                   standardGeneric("fitObject")
                 })

#----------------------------------------------------------------------#
# Method to retrieve function name                                     #
#----------------------------------------------------------------------#
setGeneric(name = "method",
           def = function(object,...){
                   standardGeneric("method")
                 })

#----------------------------------------------------------------------#
# Method to retrieve user specified inputs                             #
#----------------------------------------------------------------------#
setGeneric(name = "methodArgs",
           def = function(object,...){
                   standardGeneric("methodArgs")
                 })

#----------------------------------------------------------------------#
# Method to retrieve solver method                                     #
#----------------------------------------------------------------------#
setGeneric(name = "model",
           def = function(object,...){
                   standardGeneric("model")
                 })

#----------------------------------------------------------------------#
# Method to retrieve predict function                                  #
#----------------------------------------------------------------------#
setGeneric(name = "predictor",
           def = function(object,...){
                   standardGeneric("predictor")
                 })

#----------------------------------------------------------------------#
# Method to retrieve formal arguments to predict function.             #
#----------------------------------------------------------------------#
setGeneric(name = "predictorArgs",
           def = function(object,...){
                   standardGeneric("predictorArgs")
                 })

#----------------------------------------------------------------------#
# Method to set formal arguments of predict method.                    #
#----------------------------------------------------------------------#
setGeneric(name = "predictorArgs<-",
           def = function(object, value){
                   standardGeneric("predictorArgs<-")
                 })

#----------------------------------------------------------------------#
# Method to retrieve solver method                                     #
#----------------------------------------------------------------------#
setGeneric(name = "solver",
           def = function(object,...){
                   standardGeneric("solver")
                 })

#----------------------------------------------------------------------#
# Method to retrieve formal arguments to solver method.                #
#----------------------------------------------------------------------#
setGeneric(name = "solverArgs",
           def = function(object,...){
                   standardGeneric("solverArgs")
                 })

#----------------------------------------------------------------------#
# Method to set formal arguments of solver method.                     #
#----------------------------------------------------------------------#
setGeneric(name = "solverArgs<-",
           def = function(object,value){
                   standardGeneric("solverArgs<-")
                 })


