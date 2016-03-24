#Development file

#Set working directory
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/Midterm")

#load packages to create package
library(devtools)
library(roxygen2)


#package.skeleton(code_files = c("integrateItpackage-package.R", "integrateIt.R", "Trapezoid.R", "Simpson.R", "tolTest.R"), name="integrateItpackage")

#Create package
current.code <- as.package("integrateItpackage")
# load all functions
load_all(current.code)
# make help files
document(current.code)

#EXAMPLE
library(integrateItpackage)
a=2
b=8
x <- c(1,5,6,7,2,3,9,4,8)
y=x^2
trap <- integrateIt(x=x, y=y, a=a, b=b, rule="Trapezoid")
plot(trap)
simp <- integrateIt(x=x, y=y, a=a, b=b, rule="Simpson")
plot(simp)
fun <- function(x) x^2
tolTest(fun=fun, a=a, b=b, start=4, rule="Simpson", correct=168, tol=1)
