#' Computes the p-value for a given t-statistic and plots the t-distribution
#'
#' @param t0 Observed t-statistic.
#' @param xmax Maximum x-axis value for the plot.
#' @param n Sample size.
#' @param alpha Significance level (default is 0.05).
#'
#' @return A list containing:
#' \item{q}{Critical t-values for the given alpha.}
#' \item{pvalue}{The computed p-value for the observed t-statistic.}
#' @export
#'
mypvalue <- function(t0, xmax = 4, n = 20, alpha = 0.05) {
  # calculate alpha/2
  va <- round(pt(-t0, df = n - 1), 4)
  pv <- 2 * va

  # plot the t dist
  curve(dt(x, df = n - 1), xlim = c(-xmax, xmax), ylab = "T Density",
        xlab = expression(t), main = substitute(paste("P-value=", pv, " alpha=", alpha)))

  # set up points on the polygon to the right
  xcurve <- seq(t0, xmax, length = 1000)
  ycurve <- dt(xcurve, df = n - 1)

  # set up points to the left
  xlcurve <- seq(-t0, -xmax, length = 1000)
  ylcurve <- dt(xcurve, df = n - 1)

  # Shade in the polygon defined by the line segments
  polygon(c(t0, xcurve, xmax), c(0, ycurve, 0), col = "green")
  polygon(c(-t0, xlcurve, -xmax), c(0, ylcurve, 0), col = "green")

  # make quantiles
  q <- qt(1 - alpha / 2, n - 1)
  abline(v = c(q, -q), lwd = 2) # plot the cut-off t-value
  axis(3, c(q, -q), c(expression(abs(t[alpha / 2])), expression(-abs(t[alpha / 2]))))

  # Annotation
  text(0.5 * (t0 + xmax), max(ycurve), substitute(paste(area, "=", va)))
  text(-0.5 * (t0 + xmax), max(ycurve), expression(area))

  return(list(q = q, pvalue = pv))
}
