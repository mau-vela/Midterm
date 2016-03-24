#' Numeric integration with degree of tolerance
#' 
#' Function that increases the number of intervals until reaching a level of tolerance 
#'
#' @param fun Function .
#' @param a Lower limit
#' @param b Upper limit
#' @param tol level of tolerance
#' @param rule Simpson's Rule or the Trapezoidal Rule
#' @param correct A correct argument that provides the correct answer for the integral
#' @param start initial value of n
#' 
#' @return Returns final n, result for integration, and error
#' 
#' @author Mauricio Vela
#' @examples
#' 
#' @seealso \code{\link{Trapezoid}}, \code{\link{Simpson}}, \code{\link{integrateIt}}
#' @rdname tolTest
#' @aliases tolTest,ANY-method
#' @export
#Generic for tolTest
setGeneric(name = "tolTest",
           def=function(fun, a, b, start, tol, rule, correct, abserror, start)
           {standardGeneric("tolTest")}
)

# Method 
setMethod(f="tolTest", definition=function(fun, a, b, start, tol, rule, correct){
            
            if(rule!="Simpson" & rule!="Trapezoid") stop("Put Simpson or Trapezoid in rule")
          
            #Don't allow start==2 or less
            if (start<=2 & rule=="Trapezoid") start=3
            else if (start<=2 & rule=="Simpson") start=4
            
            #allow abserror to have initial value
            abserror <- tol+1
            
            while(abserror > tol){
              # calculate h
              h <- (b-a)/start
              # x values
              x <- sort(x)
              x <- seq(a, b, by=h)
              #applied function to x
              y <- fun(x)

              #Integration
              #Using Simpson
              if (rule == "Simpson") {
                if(start %% 2 != 0)  stop("When using Simpson's rule, n most be even")
                s <- y[1] + y[start+1] + 4*sum(y[seq(2,start,by=2)]) + 2 *sum(y[seq(3,start-1, by=2)])
                s <- s*h/3
              }
              #UsIng Trapezoid
              if (rule == "Trapezoid") {
                 s <- h * (y[1]/2 + sum(y[2:start]) + y[start+1]/2)
              }	
              abserror <- abs(s-correct)
              if (rule == "Simpson") start <- start + 2
              else start <- start+1
            }
            return(list("fun"=fun, "a"=a, "b"=b, "tolerance"=tol, "rule"=rule, 
                        "correct"=correct, "abserror"=abserror, "final n"=start))
})