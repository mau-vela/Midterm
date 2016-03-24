#Create method trapezoid that needs to have x vector , y vector , a and b limit values and the output from the integration
setClass(Class="Trapezoid", slots = c(x = "numeric", y = "numeric", a = "numeric", b = "numeric"),
         prototype = prototype( x = numeric(), y = numeric(), a = numeric(), b = numeric()), 
         validity=function(object){
           # Validity to check there are no missing arguments
           if(is.null(object@x) | is.null(object@y) | is.null(object@a) | is.null(object@b)){
             stop("All x, y, a and b need to be specified")
           }
           # make sure a and b are different and be most be greater to a
           if(index.a >= index.b){
             stop("a most be different from b, and b most be greater to a")
           }
         }
)