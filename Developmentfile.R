#Development file

#Set working directory
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/Midterm")

#load packages to create package
library(devtools)
library(roxygen2)


#EXAMPLE

a=2
b=7
x <- c(1,5,6,7,2,3,9,4,8)
y=x^2
xx <- new("Trapezoid", x=x, y=y, a=a, b=b)
