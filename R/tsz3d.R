#' Plot change in channel temperature per hour
#'
#' @param TSZnum the number of TSZs in model. numeric.
#' @param dates the date or dates to plot. lubridate object.
#' @param modelTemps list of model temperatures. tts object.
#' @param channelDepth numeric.
#' @param channelSurfaceArea numeric.
#' @param binStats TSZ geometry. dataframe output from hyporheicBins()
#' @param colors color ramp. numeric vector.
#' @param numColors the number of colors in colors vector. numeric.
#' @param colorGroupings the number of divisions in colors. numeric.
#' @param zlim z-axis upper and lower bounds. numeric vector of length 2.
#' @param phi
#' @param theta
#' @zeroGrid plot a  zero plane? TRUE/FALSE. default TRUE.
#'
#' @return 3-D surface plot of change in channel temperature per hour contributed by each TSZ.


tsz3d <- function(TSZnum,
                  dates,
                  modelTemps,
                  channelDepth,
                  channelSurfaceArea,
                  binStats,
                  colors,
                  numColors,
                  colorGroupings,
                  zlim,
                  phi = 30,
                  theta = 30,
                  zeroGrid = TRUE){
  require(plot3D)

  channelT.2 <- as.vector(coredata(modelTemps[[1]]$svValue[dates]))
  for(i in 2:(TSZnum+1)){
    assign(paste0("hyp", i-1, "T.2"), as.vector(coredata(modelTemps[[i]]$svValue[dates])))
  }

  grossHypFlux <- numeric(TSZnum)
  for(i in 1:TSZnum){
    grossHypFlux[i] <- binStats$returnFlow[i]/channelSurfaceArea
  }


  fluxRatio <- matrix(nrow = 24, ncol = TSZnum)
  for(i in 1:TSZnum){
    fluxRatio[,i] <- ((grossHypFlux[i] * ((get(paste0("hyp", i,"T.2"))) - channelT.2))/(channelDepth))*3600
  }

  fluxCol <- matrix(nrow = 24, ncol = TSZnum)
  for(c in 1:TSZnum){
    for(r in 1:24){
      for(z in 1:(numColors-1)){
        if(fluxRatio[r,c] >= colorGroupings[z] && fluxRatio[r,c] < colorGroupings[z+1]){
          fluxCol[r,c] <- colors[z]
        }}}}

  surf3D(x = matrix(seq(1, 24, 1), nrow = 24, ncol = TSZnum),
         y = matrix(rep(seq(1, TSZnum, 1), each = 24), nrow= 24, ncol = TSZnum),
         z = fluxRatio,
         theta = theta,
         phi = phi,
         d = 1,
         xlab = "Hour of Day",
         ylab = "TSZ Number",
         zlab = "Change in degC per Hour",
         main = month(dates, label = T, abbr = F),
         zlim = zlim,
         bty = "b",
         colvar = fluxCol,
         resfac = c(5,5),
         clim = c(min(colors1), max(colors1)))

if(zeroGrid){
  surf3D(x = matrix(seq(1, 24, 1), nrow = 24, ncol = TSZnum),
         y = matrix(rep(seq(1, TSZnum, 1), each = 24), nrow= 24, ncol = TSZnum),
         z = matrix(0, nrow=24, ncol = TSZnum),
         add = TRUE,
         facets = NA,
         border = "gray40",
         colkey = F)
}

}



