#' Computes a 95% confidence interval for the mean
#'
#' @param x Vector representing the sample data
#' @return Vector of length 2, for lower and upper bounds of the confidence interval
#' @examples
#' set.seed(23)
#' x <- rnorm(30, mean = 10, sd = 12)
#' myci(x)
#'
#' @export
myci <- function(x) {
  # Sample size
  n <- length(x)

  # Sample mean
  x_mean <- mean(x)

  # Sample standard deviation
  x_sd <- sd(x)

  # t-value for 95% confidence interval
  alpha <- 0.05
  t_alpha <- qt(1 - alpha / 2, df = n - 1)

  # Margin of error
  error <- t_alpha * (x_sd / sqrt(n))

  # Confidence interval
  ci_lower <- x_mean - error
  ci_upper <- x_mean + error
  ci <- c(ci_lower, ci_upper)

  return(ci)
}
