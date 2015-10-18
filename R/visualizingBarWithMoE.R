#' Visualizing Poll Data--Bar
#'
#' This function allows you to visualize the data in pollVizR. Returns a vertical barplot with margins of error embedded.
#' @param fake Your dataframe returned from poll_Import. FUTURE: List returned from poll_Import.
#' @param plotTitle String representing the title. No default.
#' @param cI *Integer* representing the confidence interval for the y-axis label. Defaults to 95.
#' @examples pollViz(fake, plotTitle = "Fake data are cool")

pollVizBar <- function(fake, plotTitle, cI=95, wes=F) {
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  if (!is.na(wes)) {
    suppressPackageStartupMessages(require(wesanderson))
    wes <- as.character(wes)
  }

  plotTitle <- as.character(plotTitle)

  interval <- aes(ymin=LowerCI, ymax=UpperCI)
  neuteufelPlot <- ggplot(data=fake, aes(x=Choices, y=Support, fill=Choices)) +
    geom_bar(stat="identity") + geom_errorbar(interval, width=0.5) +
    ggtitle(paste(plotTitle)) + xlab("Choices\n\nMade with pollVizR") + guides(fill=F) + expand_limits(y=0) +
    ylab(paste("Polled Support with a ", cI, "% confidence interval", sep="")) + scale_y_continuous(labels=percent)
  neuteufelPlot
}
