#' A Simpson object 
#' 
#' Objects of class \code{Simpson} are created using \code{integrateIt} function. 
#' 
#' An object of the class 'Simpson' has the following slots:
#' \itemize{
#' \item \code{x} Vector of x values
#' \item \code{y} Vector of y values
#' \item \code{a} Lower limit 
#' \item \code{b} Upper limit 
#' }
#'
#' @author Mauricio Vela
#'
#' @aliases Simpson-class initialize,Simpson-method print,Simpson-method plot, Simpson-method
#' @rdname Simpson
#' @export
#Create method trapezoid that needs to have x vector , y vector , a and b limit values and the output from the integration
setClass(Class="Simpson", slots = c(x = "numeric", y = "numeric", a = "numeric", b = "numeric", s = "numeric", n = "numeric"),
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
           #n must be even
           if(object@n %% 2 != 0){
             stop("When using Simpson's rule, length of x's to be used most be odd")
           }
           
         }
)


#' @export
#intialize trapezoid
setMethod("initialize", "Simpson",
          function(.Object, x, y, a, b){
            #assign objects
            .Object@x <- x
            .Object@y <- y
            .Object@a <- a
            .Object@b <- b
            
            #sort x and y in case are not sorted
            y <- y[order(x)] 
            x <- sort(x)
            yy <- y[x>=a & x<=b]
            xx <- x[x>=a & x<=b]
            
            #Create n (panels according to x vector)
            n <- length(xx[xx>=a & xx<=b])-1
            
            #Create h
            h <- (b-a)/n
            
            #create s
            #in case n=2
            if (n == 2) {
              s <-h/3* (yy[which(xx == a)] + 4*yy[which(xx == a)+1] + yy[which(xx == b)])
            } 
            # in case n greater than 2
            else {
              s <-h/3* (yy[which(xx == a)] + yy[n+1] + 2*sum(yy[seq(2,n,by=2)]) + 4 *sum(yy[seq(3,n-1, by=2)]))
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
          signature="Simpson",
          # create function
          definition=function(x){
            show(x)
          }  
)

#' @export
# plot method for object trapezoid
setMethod(f="plot", signature="Simpson",
          #start definition
          definition=function(x=NULL, y=x, ...){
            # sort x and y vectors
            YVEC <- x@y[order(x@x)]  
            XVEC <- x@x
            YVEC <- YVEC[XVEC>=x@a & XVEC<=x@b]
            XVEC <- XVEC[XVEC>=x@a & XVEC<=x@b]
            n <- x@n
            # create plot
            plot(XVEC, YVEC,  xlab = "X", ylab = "f(x)", main = "Plot with Simpson rule")
            # create n segments to show subdivisions
            segments(XVEC, rep(0,n), XVEC, YVEC, col="black", lty=2)
            #Draw parabolas
            parabolas <- function(i){
              # values between a and b
              XX <- seq(from=XVEC[i-1], to=XVEC[i+1], length.out=10)
              #Calculate p(x)
              px<- (YVEC[i-1]) * ((XX - XVEC[i])*(XX - XVEC[i+1]))/((XVEC[i-1] - XVEC[i])*(XVEC[i-1] - XVEC[i+1]))+
                (YVEC[i]) * ((XX - XVEC[i-1])*(XX - XVEC[i+1]))/((XVEC[i] - XVEC[i-1])*(XVEC[i] - XVEC[i+1]))+
                (YVEC[i+1]) * ((XX - XVEC[i-1])*(XX - XVEC[i]))/((XVEC[i+1] - XVEC[i-1])*(XVEC[i+1] - XVEC[i])) 
              lines(XX, px)
           }
            # Actually runs the function
            invisible(lapply(2:(length(XVEC)-1), FUN=parabolas))
          }  
          
)



