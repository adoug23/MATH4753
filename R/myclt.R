#' Central Limit Theorem Function
#'
#' This function creates random numbers from a uniform distribution then adds them to create a histogram of sums.
#'
#' @param n The sample size
#' @param iter The number of iterations
#' @param a The lower limit
#' @param b The upper limit
#'
#' @return A list of sums for each group of the numbers.
#' @export
#'
#' @examples
#' w <- myclt(n=50, iter=10000)
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
