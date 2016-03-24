#EXAMPLE

a=2
b=7
x <- c(1,5,6,7,2,3,9,4,8)
y=x^2
xx <- new("Trapezoid", x=x, y=y, a=a, b=b)



#' @export
#Create method trapezoid that needs to have x vector , y vector , a and b limit values and the output from the integration
setClass(Class="Trapezoid", slots = c(x = "numeric", y = "numeric", a = "numeric", b = "numeric", s = "numeric", n = "numeric"),
         prototype = prototype( x = numeric(), y = numeric(), a = numeric(), b = numeric(), s = numeric(), n = numeric()), 
         validity=function(object){
           # Validity to check there are no missing arguments
           if(is.null(object@x) | is.null(object@y) | is.null(object@a) | is.null(object@b)){
             stop("All x, y, a and b need to be specified")
           }
           # Validity to length of x and y are equal in the interval between a and b
           if(length(x[object@x>=object@a & object@x<=object@b])!=length(y[object@x>=object@a & object@x<=object@b])){
             stop("lenght in the interval of x and y most be equal")
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
            
            #sort x and y in case are not sorted
            y <- y[order(x)] 
            x <- sort(x)
                   
            
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
              s <- h/2 * (y[which(x == a)] + y[which(x == b)] + sum(2*y[(which(x == a)+1):(which(x == b)-1)]))
            }
            #object s save
            .Object@s <- s
            #object n save
            .Object@n <- n

            value=callNextMethod()
            return(value)
          }
) 

#' @export
#print for trapezoid
setMethod(f="print",
          signature="Trapezoid",
          # create function
          definition=function(x){
            show(x)
          }  
)

#' @export
# plot method for object trapezoid
setMethod(f="plot", signature="Trapezoid",
          #start definition
          definition=function(x=NULL, y=x, ...){
            # sort x and y vectors
            Y <- x@y[order(x@x)]  
            X <- x@x
            Y <- Y[X>=x@a & X<=x@b]
            X <- X[X>=x@a & X<=x@b]
            n <- x@n
            # create plot
            plot(X, Y,  xlab = "X", ylab = "f(x)", main = "Plot with Trapezoid rule")
            # create trapezoid line overtop "function"
            lines(X, Y, col="red")
            # create n segments to show subdivisions
            segments(X, rep(0,n), X, Y, col="black", lty=2)
          }  
)
