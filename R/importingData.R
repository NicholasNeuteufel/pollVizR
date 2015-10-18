#' Importing Poll Data for Use in pollVizR
#'
#' 0.1: This function allows you to import the data for use in pollVizR. Returns a dataframe with the choices, support, standard error, and selected confidence interval as well as an introductory return sentence.
#' FUTURE: This function returns a list with a dataframe in [1], n in [2], and cI in [3] (reduces work in other functions).
#' Run this before any other function in pollVizR.
#' @param choices Vector representing candidate/party names. Defaults to "C1", "C2", and "Undecided."
#' @param support Vector of percentage support. No default.
#' @param n Number of poll respondents. No default.
#' @param cI Confidence interval. Select from 0.95 (default), 0.8, 0.85, 0.9, and 0.99.
#' @param decimal Is ``support" a decimal? If so, TRUE (default). If percentage, FALSE.
#' @examples fake <- poll_Import(choices=c("A", "B", "C"), support=c(0.45, 0.45, 0.05), n=250)
#' @note Important to save as an object for pollViz.

poll_Import <- function(choices=c("C1","C2","Undecided"), support, decimal=T, n, cI=0.95) {
  #Warning messages
  if (length(choices) != length(support)) {
    print("Your choices and support vectors do not match. You're going to have a bad time.")
  }

  #Convert to decimal if necessary
  if (decimal==F) {
    for (i in 1:length(support)) {
      support[i] <- support[i]/100
    }
  }

  #Define some vectors
  q <- vector(mode="numeric", length=length(support))
  se <- vector(mode="numeric", length=length(support))
  cIUpper <- vector(mode="numeric", length=length(support))
  cILower <- vector(mode="numeric", length=length(support))

  #Calculate the standard error
  for (i in 1:length(support)) {
    q[i] <- 1-support[i]
    se[i] <- sqrt(support[i]*q[i])/sqrt(n-1)
  }

  #Set standard error coefficient
  if (cI==0.95) {
    x <- 1.96
  }
  else if (cI==0.9) {
    x <- 1.645
  }
  else if (cI==0.8) {
    x <- 1.28
  }
  else if (cI==0.85) {
    x <- 1.44
  }
  else if (cI==0.99) {
    x <- 2.575
  }
  else {
    print("Sorry. This version does not support that interval choice. It supports 0.8, 0.85, 0.9, 0.95, and 0.99.")
  }

  for (i in 1:length(support)) {
    cILower[i] <- (-x*se[i]) + support[i]
    cILower[i] <- round(cILower[i], 3)
    cIUpper[i] <- (x*se[i]) + support[i]
    cIUpper[i] <- round(cIUpper[i], 3)
  }

  for (i in 1:length(support)) {
    se[i] <- round(se[i], 3)
  }

  pollResults <- data.frame(Choices = as.factor(choices), Support = as.numeric(support), SE = as.numeric(se), UpperCI = as.numeric(cIUpper), LowerCI = as.numeric(cILower))
  #pollResults <- list(pollResults, n, cI)
  return(pollResults)
}
