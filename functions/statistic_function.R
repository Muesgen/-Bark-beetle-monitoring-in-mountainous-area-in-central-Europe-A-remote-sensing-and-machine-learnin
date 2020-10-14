#### functions for calculationg raster sattistics
#functions can be parallelice with clusterR() https://cran.rstudio.com/web/packages/raster/raster.pdf#page.50

## 75th quantile
fq75 <-function(x) {
  calc(x, fun = function(a) {quantile(a,probs = .75,na.rm=TRUE)})
}

## 50th quantile
fq50 <- function(x) {
  calc(x, fun = function(a) {quantile(a,probs = .5,na.rm=TRUE)})
}

## 25th quantile
fq25 <- function(x) {
  calc(x, fun = function(a) {quantile(a,probs = .25,na.rm=TRUE)})
}

## standard deviation

fqsd <- function(x) {
  calc(x, fun=sd, na.rm=TRUE)
}