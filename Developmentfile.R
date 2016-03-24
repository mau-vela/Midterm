#Development file

#Set working directory
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/Midterm")

#load packages to create package
library(devtools)
library(roxygen2)



#Create method trapezoid that needs to have x vector , y vector , a and b limit values and the output from the integration
setClass(Class="Trapezoid", slots = c(x = "numeric", y = "numeric", a = "numeric", b = "numeric", n= "numeric" ),
         prototype = prototype( x = numeric(), y = numeric(), a = numeric(), b = numeric(), n = numeric()), 
        validity=function(object){
          # Validity to check there are no missing arguments
          if(is.null(object@x) | is.null(object@y) | is.null(object@a) | is.null(object@b)){
             stop("All x, y, a and b need to be specified")
           }
           # make sure n 
           if(object@n %% 1 != 0 | object@n < 0){
             stop("N must be a positive integer")
           }
         }
)