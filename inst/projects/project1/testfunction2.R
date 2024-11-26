#' Optimal Number of Tickets to Sell
#'
#' @description This function calculates the optimal number of tickets to sell for a flight.
#'
#' @param N Number of seats available on the flight.
#' @param gamma Acceptable risk of overbooking (probability).
#' @param p Probability that a given passenger shows up.
#'
#' @return Returns requirements listed in project 1.
#' @export
#'
#' @examples
#' nticketresult <- ntickets(400, 0.02, 0.95)
#' print(nticketresult)
#'
ntickets = function(N, gamma, p) {

  #Cumualtive distribution function for testing to get risk of overbooking - pbinom
  nddistribution = function(n) {
    1 - gamma - pbinom(N, n, p)
  }

  #Using nd for discrete distributio
  possibleValues = seq(N, N + 50, by = 1)
  #Applies the function to each element of possibleValues
  abs_diff_discrete = abs(sapply(possibleValues, nddistribution))
  #To find the n with the minimum objective value
  nd = possibleValues[which.min(abs_diff_discrete)]

  #Normal approximation function with continuity correction from chapter 6
  ncdistribution = function(n) {
    1 - gamma - pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p)))
  }

  #Using nc for normal approximation
  nc = optimize(function(n) abs(ncdistribution(n)), interval = c(N, N + 50))$minimum

  #Plotting values for objective function for discrete and continuous
  objdiscrete = sapply(possibleValues, nddistribution)
  objnormal = sapply(possibleValues, ncdistribution)

  #Creating limits on x axis similar to the graph given in project canvas file
  x_max = max(nd, nc) + 20  #Seems without 20, the graph goes to far on x axis
  x_min = N  #Starting at given value

  y_max_discrete = max(objdiscrete) + 0.1  #Similar to x axis was handled
  y_max_continuous = max(objnormal) + 0.1

  #Plotting the discrete values, blue dots with red crosshair
  par(mfrow = c(2, 1))  #Aligning plots
  plot(possibleValues, objdiscrete, type = "b", col = "blue", pch = 19, xlab = "n", ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold\n(", round(nd, 4), ") gamma=", gamma, " N=", N, " discrete"),
       xlim = c(x_min, x_max), ylim = c(min(objdiscrete), y_max_discrete))  #Using previously made limits

  #Crosshair lines to the nd value
  abline(v = nd, col = "red", lwd = 2)
  abline(h = nddistribution(nd), col = "red", lwd = 2)

  #Plotting the continous line, black line with blue crosshair
  plot(possibleValues, objnormal, type = "l", col = "black", lwd = 2, xlab = "n", ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold\n(", round(nc, 4), ") gamma=", gamma, " N=", N, " continuous"),
       xlim = c(x_min, x_max), ylim = c(min(objnormal), y_max_continuous)) #similar limits as before

  #Crosshair for nc value
  abline(v = nc, col = "blue", lwd = 2)  # Vertical line at nc
  abline(h = ncdistribution(nc), col = "blue", lwd = 2)  # Horizontal line at the objective function value at nc

  #Returning the list of values
  returnList = list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  return(returnList)
}

#Example:
nticketresult = ntickets(200, 0.02, 0.95)
print(nticketresult)
