#' Statistics and Plots
#'
#' Creates a histogram and a boxplot of the given numeric vector and returns basic statistics.
#' @param x Numeric vector.
#' @return A data frame of basic statistics.
#' @export
stats <- function (x) {
  # Set up the plotting area to have 1 row and 2 columns
  par(mfrow = c(1,2))

  # Create a histogram of the data with rainbow colors
  hist(x, col = rainbow(30))

  # Create a boxplot of the data with blue color
  boxplot(x, col = 'blue')

  # Reset the plotting area to default configuration
  par(mfrow = c(1,1))

  # Return the data frame of statistics
  data.frame(min = min(x),
             median = median(x),
             mean = mean(x),
             max = max(x),
             sd = sd(x),
             range = max(x) - min(x))
}
