#' Title: myncurve function implementation
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a a value
#'
#' @return A list of the mean and standard deviation
#' @export
#'
#' @examples
#' myncurve(0, 1, 2)
#'
myncurve=function(mu, sigma, a){

  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        main = paste("Normal Curve with mu =", mu, "and sigma =", sigma))

  xcurve <- seq(mu - 3 * sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col = "red")

  area <- pnorm(a, mean = mu, sd = sigma)

  return(list(mu = mu, sigma = sigma, area = area))
}
