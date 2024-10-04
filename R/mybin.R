#' Title: Binomial Distribution Simulation
#'
#' @param iter iteration variable
#' @param n number of samples
#' @param p probability of success
#'
#' @return A barplot of the proportions of the successes
#' @export
#'
#' @examples
mybin=function(iter=100,n=10, p=0.7){
  # This is the function that will simulate a binomial distribution
  # The top portion is adjustable when the function is called
  # iter is for the number of iterations, n is for the sample, and p is the probability
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  # This part creates a matrix to hold the samples
  succ=c()
  for( i in 1:iter){
    # Using the empty vector the for loop goes through the iterations
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  # Success is based on p while the failures are based on 1-p
  # The successes are tracked and the following will create a table of the successes
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  # This will create a barplot of the proportions of the successes
  succ.tab/iter
}
