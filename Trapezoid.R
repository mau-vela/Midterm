#' @export
#Create method trapezoid that needs to have x vector , y vector , a and b limit values and the output from the integration
setClass(Class="Trapezoid", slots = c(x = "numeric", y = "numeric", a = "numeric", b = "numeric", s = "numeric"),
         prototype = prototype( x = numeric(), y = numeric(), a = numeric(), b = numeric(), s = numeric()), 
         validity=function(object){
           # Validity to check there are no missing arguments
           if(is.null(object@x) | is.null(object@y) | is.null(object@a) | is.null(object@b)){
             stop("All x, y, a and b need to be specified")
           }
           # make sure a and b are different and be most be greater to a
           if(object@a >= object@b){
             stop("a most be different from b, and b most be greater to a")
           }
           # make sure a and b are included in x
           if(!(object@a %in% object@x & object@b %in% object@x)){
             stop("a and b most correspond to values of x")
           }           
         }
)


#' @export
#intialize trapezoid
setMethod("initialize", "Trapezoid",
          function(.Object, x, y, a, b){
            #assign objects
            .Object@x <- x
            .Object@y <- y
            .Object@a <- a
            .Object@b <- b
            
            #Create n (panels according to x vector)
            n <- length(x[x>=a & x<=b])-1

            #Create h
            h <- (b-a)/n

            #create s
            #in case n=1
            if(n == 1){
              s <- h/2 * (y[which(x == a)] + y[which(x == b)])
            }
            #when cases more than one
            if(n > 1){
              s <- h/2 * (y[which(x == a)] + y[which(x == a)] + sum(2*y[(which(x == a)+1):(which(x == a)-1)]))
            }
            #object s save
            .Object@s <- s

            value=callNextMethod()
            return(value)
          }
) 