

#' Define inverse normal axis
#'
#' Function to define inverse normal axis for frequency curves
#'
#' @import scales
#' @export
inv_norm_trans <- function(){
  scales::trans_new(
    name="inv_norm",
    transform = function(x) -qnorm(x),
    inverse = function(x) pnorm(-x)
  );


}



#' Plot Flood Frequency Analysis Curve
#'
#' Function to plot results of peak-flow frequency analysis
#'
#' @param plotPositions dataframe containing observed peak values in a column named "peak_va",corresponding plotting positions in a column named "plotPosition",
#'              and an optional column named "historic" containing a logical TRUE for peaks which are historic
#' @param LP3fit dataframe containing LP3 curve fit for a site with exceedance probabilities in a column named "EXC_Prob", and corresponding peak estimates in a column named "Estimate",
#'        lower confidence limits in a column named "Conf_Low", and upper confidence limits in a column named "Conf_Up"
#' @param outFile file path to output graph
#' @param siteNum character - site number to print on plot
#' @param siteName character - site name to print on plot
#' @param PILF numeric - PILF threshold for analysis
#'
#' @import ggplot2
#' @import scales
#'
#' @export
#'
ffaPlot <- function(plotPositions, LP3fit, outFile, siteNum="", siteName="", PILF = 0){

  #Set logical for whether each peak is historic
  if(!is.null(plotPositions$historic)){
    plotPositions$historic[is.na(plotPositions$historic)] <- FALSE
    plotPositions$historic[plotPositions$historic != TRUE] <- FALSE
  }
  else{
    plotPositions$historic <- FALSE
  }


  #Handle censored peaks
  plotPositions$censored <- FALSE
  plotPositions$censored[plotPositions$ql != plotPositions$qu] <- TRUE
  #Pull censored peaks into their own dataframe
  censoredPeaks <- plotPositions[plotPositions$censored, c("plotPosition", "ql", "qu")]
  plotPositions <- plotPositions[!plotPositions$censored, ]

  #Separate systematic peaks from PILFs
  plotPositions$gagedPeaks <- NA
  plotPositions$PILFs <- NA
  plotPositions$historicPeaks <- NA
  plotPositions[plotPositions$peak_va < PILF, "PILFs"] <- plotPositions[plotPositions$peak_va < PILF, "peak_va"]
  plotPositions[plotPositions$peak_va >= PILF & !plotPositions$historic & !plotPositions$censored, "gagedPeaks"] <- plotPositions[plotPositions$peak_va >= PILF & plotPositions$historic != TRUE & !plotPositions$censored, "peak_va"]
  plotPositions[plotPositions$peak_va >= PILF & plotPositions$historic & !plotPositions$censored, "historicPeaks"] <- plotPositions[plotPositions$peak_va >= PILF & plotPositions$historic == TRUE & !plotPositions$censored, "peak_va"]

  #Remove portion of frequency curve less than PILF threshold

  LP3fit$Estimate[LP3fit$Estimate < PILF] <- NA
  LP3fit$Conf_Low[is.na(LP3fit$Estimate)] <- NA
  LP3fit$Conf_Up[is.na(LP3fit$Estimate)] <- NA

  #Set up AEPs and AEP labels for the plot
  AEPs <- c(0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.98, 0.99, 0.995)
  AEPlabels <- c("0.2", "", "1", "", "5", "10", "20", "", "", "40", "", "60", "", "75", "", "90", "95", "98", "", "99.5")

  #Calculate the number of axis breaks needed

  maxBreak <- ceiling(log10(max(plotPositions$peak_va, LP3fit$Estimate, LP3fit$Conf_Low, LP3fit$Conf_Up, na.rm = TRUE))) #Get max break on the axis
  minBreak <- floor(log10(min(plotPositions$peak_va, LP3fit$Estimate, LP3fit$Conf_Low, LP3fit$Conf_Up, na.rm = TRUE))) #Get min break on the axis

  minBreak <- max(minBreak, -1, na.rm=TRUE) #If a zero value is in the plotted data, set minBreak to -1

  breakPoints <- 10^seq(minBreak, maxBreak) #Set breaks at integer powers of 10

  #Set number of decimal places to show on scale
  if(minBreak < 0){
    scaleAccuracy <- 0.1
  }
  else{
    scaleAccuracy <- 1
  }

  xTitle <- paste("Annual exceedance probability, in percent \n Station - ", siteNum, siteName)

  peakFlowPlot <- ggplot() +
    geom_point(data=plotPositions, aes(plotPosition, gagedPeaks, color="Gaged Peak Discharge"), size=2) + #Plot systematic peaks
    geom_point(data=plotPositions, aes(plotPosition, historicPeaks, color="Historic Peak"), shape=17, size=2) + #Plot historic peaks
    geom_point(data=plotPositions, aes(plotPosition, PILFs, color="PILF"), shape=21, size=2, fill="white", stroke=1) + #Plot PILFs
    geom_errorbar(data=censoredPeaks, aes(plotPosition, ymin=pmax(ql, 10^minBreak), ymax=qu), width=0.05, position=position_dodge(0.05)) + #Plot censored data
    geom_line(data=LP3fit, aes(EXC_Prob, Estimate, color="Fitted Frequency Curve"),  size=1) +
    geom_line(data=LP3fit, aes(EXC_Prob, Conf_Low, color="Confidence limits"),  size=1) +
    geom_line(data=LP3fit, aes(EXC_Prob, Conf_Up, color="Confidence limits"), size=1) +
    scale_x_continuous(trans="inv_norm", breaks=AEPs, labels=AEPlabels, limits=c(0.995, 0.002)) +
    scale_y_continuous(trans="log10", breaks = breakPoints, limits=10^c(minBreak, maxBreak), labels = scales::comma_format(accuracy = scaleAccuracy)) +
    xlab(xTitle) +
    ylab("Annual peak discharge, in cubic feet per second") +
    theme_light() +
    theme(axis.text=element_text(face="bold"), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"), legend.position = "bottom") +
    scale_color_manual(values=c("Gaged Peak Discharge"="cyan", "PILF"="black", "Historic Peak" = "purple", "Fitted Frequency Curve"="red", "Confidence limits"="blue"),
                       guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "blank", "solid", "solid"), shape=c(16,21,17,NA,NA)))) +
    labs(color= "Explanation:") +
    annotation_logticks(sides="lr") #Add minor ticks to left and right sides

  print(peakFlowPlot)
  print(outFile)

  ggsave(outFile, peakFlowPlot, width=9, height=6, units="in", dpi=600)

}
