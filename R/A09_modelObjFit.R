setClass("modelObjFit", 
         slots = c(fitObj = "ANY",
                   model  = "formula",
                   func   = "methodObjPredict"))

setMethod(f = "fit",
          signature = c(  object = "modelObj", 
                            data = "data.frame", 
                        response = "vector"),
          definition = function(object, data, response, ...) {

                         fitObj <- .fit(object = object@solver, 
                                        data = data, 
                                        response = response, 
                                        model = object@model)

                         ft <- new("modelObjFit",
                                   fitObj = fitObj, 
                                   model = object@model,
                                   func = object@predictor)

                         return(ft)
                       })

setMethod(f = "fitObject", 
         signature = c(object="modelObjFit"), 
         definition = function(object,...){
                        return(object@fitObj)
                      })

setMethod(f = "model", 
         signature = c(object="modelObjFit"), 
         definition = function(object,...){
                        return( object@model )
                      })

setMethod(f = "predictor", 
         signature = c(object="modelObjFit"), 
         definition = function(object,...){
                        return( object@func@method)
                      })

setMethod(f = "predictorArgs", 
         signature = c(object="modelObjFit"), 
         definition = function(object,...){
                        return(object@func@methodArgs)
                      })

setMethod(f="coef",
          signature = c(object="modelObjFit"),
          definition = function(object,...){
                         tmp <- try(coef(object@fitObj,...), silent=FALSE)
                         if( class(tmp) == 'try-error' ) {
                           warnMsg("coef", class(object@fitObj))
                           return(NULL)
                         } else {
                           return(tmp)
                         }
                       })

setMethod(f = "plot",
          signature = c(x="modelObjFit"),
          definition = function(x, ...){
                         tmp <- try(plot(x@fitObj, ...), 
                                    silent=TRUE )
                         if( class(tmp) == 'try-error' ) {
                           warnMsg("plot", class(x@fitObj))
                         }
                       })

setMethod(f = "print",
          signature = c(x="modelObjFit"),
          definition = function(x){
                         print(x@fitObj)
                       })

setMethod(f = "predict",
          signature = c(object="modelObjFit"),
          definition = function(object, newdata, ...){

                         if( missing(newdata) ) {
                           res <- .predict(object = object@func,
                                           fitObj = object@fitObj)
                         } else {
                           res <- .predict(object = object@func,
                                           newdata = newdata,
                                           fitObj = object@fitObj)
                         }
                         return( res )
                       })
                       
setMethod(f = "residuals",
          signature = c(object="modelObjFit"),
          definition = function(object, ...){

                         tmp <- try(residuals(object@fitObj, ...), 
                                    silent=TRUE)

                         if( class(tmp) == 'try-error' ||
                             is(tmp,"NULL") ) {
                           warnMsg("residuals", 
                                   class(object@fitObj))
                         } else {
                           matrix(data = tmp, 
                                  ncol = 1L, 
                                  dimnames=list(NULL, "residuals"))
                         }
                       })

setMethod(f = "show",
          signature = c(object="modelObjFit"),
          definition = function(object){
                         show(object@fitObj)
                       })

setMethod(f = "summary",
          signature = c(object="modelObjFit"),
          definition = function(object,...){
                         s1 <- try(summary(object@fitObj,...), 
                                   silent=TRUE)
                         if( any(class(s1) == 'try-error') ) {
                           warnMsg("summary", class(object@fitObj))
                         } else {
                           return(s1)
                         }
                       })


