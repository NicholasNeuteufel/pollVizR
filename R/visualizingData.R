#' Visualizing Poll Data
#'
#' This function allows you to visualize the data in pollVizR. Returns a ggplot with margins of error embedded.
#' @param fake Your dataframe returned from pollImport.
#' @param plotTitle String representing the title. No default.
#' @param cI *Integer* representing the confidence interval for the y-axis label. Defaults to 95.
#' @param wes FUTURE. You may pass on a string representing a Wes Anderson palette from the package ``wesanderson'' here. FUTURE
#' @examples pollViz(fake, cI = 85, plotTitle = "Fake data are cool")

pollViz <- function(fake, cI=95, plotTitle, wes=F) {
  require(ggplot2)
  require(scales)
  if (!is.na(wes)) {
    require(wesanderson)
    wes <- as.character(wes)
  }
  interval <- aes(ymin=LowerCI, ymax=UpperCI)
  plotTitle <- as.character(plotTitle)
  neuteufelPlot <- ggplot(data=fake, aes(x=Choices, y=Support, colour=Choices)) +
    geom_point (size=7, shape=23, solid=T) + geom_errorbar(interval, width=0.2) +
    ggtitle(paste(plotTitle)) + xlab("Choices\n\nMade with pollVizR by Nick Neuteufel") + guides(colour=F) +
    ylab(paste("Polled Support with a ", cI, "% confidence interval", sep="")) + scale_y_continuous(labels=percent)
  neuteufelPlot
}
