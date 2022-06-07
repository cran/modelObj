# Generate Warning that Method Does not Exist
#
# Function generates and prints warning message when a method is not defined
#   for a regression method.
#
# @name warnMsg
#
# @param x character name of method requested
#
# @param cx character name of the class of the object for which the method
#   was requested
#
# @keywords internal
warnMsg <- function(x, cx) {
  message(x, " method is not available for for an object of class ", cx)
}
