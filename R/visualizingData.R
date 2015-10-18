#' Visualizing Poll Data
#'
#' This function allows you to visualize the data in pollVizR. Returns a ggplot with margins of error embedded.
#' @param fake Your dataframe returned from poll_Import. FUTURE: List returned from poll_Import.
#' @param plotTitle String representing the title. No default.
#' @param cI *Integer* representing the confidence interval for the y-axis label. Defaults to 95.
#' @param vert Boolean. If TRUE (default), the plot will come up vertically. If FALSE, horizontally.
#' @examples pollViz(fake, plotTitle = "Fake data are cool")

pollViz <- function(fake, plotTitle, cI=95, vert=T, wes=F) {
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  if (!is.na(wes)) {
    suppressPackageStartupMessages(require(wesanderson))
    wes <- as.character(wes)
  }

  plotTitle <- as.character(plotTitle)

  if (vert==T) {
    interval <- aes(ymin=LowerCI, ymax=UpperCI)
  neuteufelPlot <- ggplot(data=fake, aes(x=Choices, y=Support, colour=Choices)) +
    geom_point (size=4, shape=23, solid=T) + geom_errorbar(interval, width=0.5) +
    ggtitle(paste(plotTitle)) + xlab("Choices\n\nMade with pollVizR") + guides(colour=F) + expand_limits(y=0) +
    ylab(paste("Polled Support with a ", cI, "% confidence interval", sep="")) + scale_y_continuous(labels=percent)
  neuteufelPlot
  }
  else {
    interval <- aes(xmin=LowerCI, xmax=UpperCI)
    neuteufelPlot <- ggplot(data=fake, aes(y=Choices, x=Support, colour=Choices)) +
      geom_point (size=4, shape=23, solid=T) + geom_errorbarh(interval, width=0.5) +
      ggtitle(paste(plotTitle)) + ylab("Choices") + guides(colour=F) + expand_limits(x=0) +
      xlab(paste("Polled Support with a ", cI, "% confidence interval\n\nMade with pollVizR", sep="")) + scale_x_continuous(labels=percent)
    neuteufelPlot
  }
}
