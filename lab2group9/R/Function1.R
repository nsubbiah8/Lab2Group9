library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Read in data to dataframe
data <- read.csv("data/DRG_data.csv")


# Function 1

#' This function makes a boxplot of payments by DRG code for either the average
#' Medicare payments, the average total payment, or the average covered charges.
#'
#' @param output_type a string that determines what category the plot is of
#'
#' @return A boxplot for payments by DRG code
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom tidyverse summarise
#'
#' @examples
#' payment("medicare")
#'
payment <- function(output_type)
{
  # Group by each of the three categories
  medicare <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Medicare.Payments))
  total_payment <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Total.Payments))
  covered_charges <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Covered.Charges))


  # Check which category the user wants to plot
  if (output_type == "medicare"){

    # Plot it
    p <- ggplot(medicare,
                aes(x=DRG.Definition, y=mean)) +
      geom_boxplot() +
      xlab("DRG Codes") +
      ylab("Average Medicare Payment (in dollars)") +
      ggtitle("Average Medicare Payment by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }
  else if (output_type == "total_payment"){
    p <- ggplot(total_payment,
                aes(x=DRG.Definition, y=mean)) +
      geom_boxplot() +
      xlab("DRG Codes") +
      ylab("Average Total Payment (in dollars)") +
      ggtitle("Average Total Payment by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }
  else{
    p <- ggplot(covered_charges,
                aes(x=DRG.Definition, y=mean)) +
      geom_boxplot() +
      xlab("DRG Codes") +
      ylab("Average Covered Charges (in dollars)") +
      ggtitle("Average Covered Charges by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }


}

# Define the function 2
#'
#' Function calculates either the mean, median, or standard deviation of the DRG
#' codes. over all of the DRG codes for average Medicare payments
#'
#' @param statistics is a string which specifies what statistic is needed
#'
#' @return returns the chosen statistic calculated
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom tidyverse summarise
#'
#' @examples
#' f2("mean")
#'
f2 <- function(statistics){
  # Assign different number to indicate the location of the statistics
  if(statistics == "mean"){
    i <- 2
  }else if(statistics == "medium"){
    i <- 3
  }else{
    i <- 4
  }
  da1 <- data %>%
    # Calculate the mean, medium, and standard deviation of each group
    group_by(DRG.Definition) %>%
    summarise(Mean = mean(Average.Medicare.Payments),
              Medium = median(Average.Medicare.Payments),
              Standard_Deviation  = sd(Average.Medicare.Payments))
  #  Replace (.) with spaces
  colnames(da1) <- gsub("\\.", " ", colnames(da1))
  #  Replace (_) with spaces
  colnames(da1) <- gsub("\\_", " ", colnames(da1))
  # return the statistics that we choose
  output2 <- knitr::kable(da1[,c(1,i)])
  return(output2)
}

