########################################################################
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Class methodObj and its methods                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# methodObj contains a function name and a list of arguments to be     #
# passed to the function on call.                                      #
#                                                                      #
#   method - ANY function name                                         #
#                                                                      #
#   methodArgs - list of formals of method to be passed to method when #
#                called                                                #
########################################################################

checkMethodObj <- function(object){

  if( !is(object@method, "character") && !is(object@method,"function") ) {
    msg <- "inappropriate type for methodObj method"
    return(msg)
  }

  nms <- names( object@methodArgs )
  fmls <- names( formals( object@method ) )

  tst <- nms %in% fmls

  if( !all(tst) ) {
    if( !("..." %in% fmls) ) {
      msg <- paste(nms[!tst], "not found in formal arguments of method")
      return(msg)
    }
  }

  return(TRUE)

}

setClass("methodObj", 
         slots = c(    method = "ANY",
                   methodArgs = "list"),
         validity = checkMethodObj )

setMethod(f = "method", 
          signature = c(object="methodObj"), 
          definition = function(object,...){
                         return(object@method)
                       })

setMethod(f = "methodArgs", 
          signature = c(object="methodObj"), 
          definition = function(object,...){
                         return(object@methodArgs)
                       })

setGeneric(name = "methodArgs<-",
           def = function(object,value){
                   standardGeneric("methodArgs<-")
                 })

setMethod(f = "methodArgs<-",   
          signature = c(object = "methodObj",
                        value = "list"), 
          definition = function(object, value){
                         nms <- names(object@methodArgs)
                         nmsNew <- names(value)

                         fmls <- names( formals( object@method ) )
                         tst <- nmsNew %in% fmls

                         if( !all(tst) && !("..." %in% fmls) ) {
                           stop(paste(nmsNew[!tst], "not found in formal arguments of method"))
                         }

                         newArgs <- object@methodArgs

                         for( i in 1L:length(nmsNew) ) {
                           newArgs[[ nmsNew[i] ]] <- value[[i]]
                         }

                         object@methodArgs <- newArgs

                         return(object)
                       })

