#' Numeric integration
#'
#' Function creates an object of class 'Simpson' or 'Trapezoid' and estimatest numeric integration
#'
#' @param x Vector containing x values
#' @param y Vector containing y values 
#' @param rule specified rule (Trapezoid or Simpson)
#'
#' @return An object of class 'Simpson' or 'Trapezoid'
#' 
#' \itemize{
#' \item \code{x} Vector containing x values 
#' \item \code{y} Vector containing x values
#' \item \code{a} Lower limit of integration
#' \item \code{b} Upper limit of integration 
#' \item \code{rule} Specified rule (Trapezoid or Simpson).
#' }
#' @author Mauricio Vela
#' @examples
#' 
#' a=2
#' b=8
#' x <- c(1,5,6,7,2,3,9,4,8)
#' y=x^2
#' integrateIt(x=x, y=y, a=a, b=b, rule="Trapezoid")
#' integrateIt(x=x, y=y, a=a, b=b, rule="Simpson")
#' 
#' @seealso \code{\link{Simpson}} \code{\link{Trapezoid}}
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
# Generic for integrateIt
setGeneric(name = "integrateIt",
           def=function(x, y, a, b, rule)
           {standardGeneric("integrateIt")}
)

# Method for integrateIt
setMethod(f="integrateIt",  definition=function(x, y, a, b, rule){
            # return object of class Simpson
            if(rule == "Simpson"){
              return(new("Simpson", x = x, y = y, a = a, b = b))
            }
            # return object of class Trapezoid
            if(rule == "Trapezoid"){
              return(new("Trapezoid", x = x, y = y, a = a, b = b))
            }
          }
)