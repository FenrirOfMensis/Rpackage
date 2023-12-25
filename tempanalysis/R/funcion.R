#' Calculate differences in sea temperatures by month for different years and depths
#'
#' @param data dataframe to analyze
#' @param year year to analyze
#' @param period period to analyze
#' @param depth depth to analyze
#'
#' @return A list with different information about the differences in temperature and average temperatures.
#' @export


complete <- function(data, year, period, depth) {
  
  par(mfrow = c(2, 1))
  #Selecting specific dataset with year given
  subset_data = data[data$Year == year, c("Month", depth)]
  
  #Using diff to compute differeces
  subset_data$Difference = c(NA, diff(subset_data[, depth]))
  
  #Printing average of the year and eliminating last row
  if (subset_data$Month[nrow(subset_data)] == "average annual") {
    
    average = paste("The average temperature for year", year, "is", subset_data[, depth][nrow(subset_data)])
    
    subset_data = subset_data[-nrow(subset_data), ]
  }
  
  #Plotting
  barplot(subset_data$Difference, 
          main = paste("Month-to-Month differences for year", year, "and", depth),
          xlab = "Month", 
          ylab = "Variation",
          names.arg = subset_data$Month,
          col = "skyblue")
  
  #Separation of datasets
  year_data <- data[data$Year == year, c("Month", depth)]
  period_data <- data[data$Year == period, c("Month", depth)]
  
  #Renaming of columns
  year_col_name <- paste("Year_", year, "_", gsub("\\s", "_", depth), sep = "")
  
  pivot <- paste(gsub("[-.]", "_", period), "_", gsub("\\s", "_", depth), sep = "")
  
  period_col_name = gsub(" ", "_", pivot)
  
  #Merging of both datasetrs
  merged_data <- data.frame(
    Month = year_data$Month,
    setNames(data.frame(year_data[, depth], period_data[, depth]), c(year_col_name, period_col_name)))
  
  #Computing differences
  merged_data$Difference <- merged_data[[year_col_name]] - merged_data[[period_col_name]]
  
  #Printing averages and eliminating the last row of the merged dataset
  if (merged_data$Month[nrow(merged_data)] == "average annual") {
    
    year_average = paste("The average temperature for year", year, "is", merged_data[, year_col_name][nrow(merged_data)])
    
    period_average = paste("The average temperature for the", period, "is", merged_data[, period_col_name][nrow(merged_data)])
    
    merged_data = merged_data[-nrow(merged_data), ]
  }
  
  #Last column as vector to be able to plot it
  merged_data$Difference = as.vector(merged_data$Difference)
  
  #Plots
  chart2 = barplot(merged_data$Difference, 
                   main = paste("Year", year, "and same month in", period, "differences"),
                   xlab = "Month", 
                   ylab = "Year",
                   names.arg = merged_data$Month,
                   col = "skyblue")
  
  par(mfrow = c(1, 1))
  
  return(list(data_first_function = subset_data, data_second_function = merged_data, year_average = year_average, period_average = period_average))
  
}