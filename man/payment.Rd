% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Function1.R
\name{payment}
\alias{payment}
\title{This function makes a boxplot of payments by DRG code for either the average
Medicare payments, the average total payment, or the average covered charges.}
\usage{
payment(output_type)
}
\arguments{
\item{output_type}{a string that can take values - medicare, total_payment
and covered_charges}
}
\value{
A boxplot for payments by DRG code
}
\description{
This function makes a boxplot of payments by DRG code for either the average
Medicare payments, the average total payment, or the average covered charges.
}
\examples{
payment("medicare")

}
