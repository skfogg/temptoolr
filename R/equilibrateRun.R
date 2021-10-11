#' Find equilibrium temperatures on January 1
#'
#' @param odbcConnection RODBC object
#' @param binStats data frame returned from hyporheicBins()
#' @param channelVolume numeric
#' @param channelSurfaceArea numeric
#' @param surfaceShade numeric
#' @param firstBin numeric
#' @param lastBin numeric
#' @param timeStep numeric
#' @param runID character
#' @param internal print java output in console? logical
#' @param startWith values other that 1 deg C to start with
#'
#' @return data.frame of equilibrized temperatures
#' @export

equilibrateRun <- function(odbcConnection,
                           binStats,
                           channelVolume,
                           channelSurfaceArea,
                           surfaceShade,
                           firstBin,
                           lastBin,
                           timeStep,
                           runID,
                           internal = FALSE,
                           startWith = NULL){
  require(RODBC)
  require(lubridate)
  require(zoo)
  require(xts)
  require(temptool)

  nbins <- (lastBin - firstBin) + 1
  aquiferVolume <- sum(binStats[firstBin:lastBin,]$aquiferStorage)

  setSkeleton(firstBin, lastBin, odbcConnection)
  setParameters(firstBin, lastBin, odbcConnection, initTemps = startWith, surfaceShade, channelSurfaceArea, channelVolume, binStats)

  setTiming(odbcConnection,
            timeStep,
            yearsToRun = 8,
            outputInterval = 4*(86400*365.25))


  # CREATE INIT TEMPS DATAFRAME
  initTemps <- data.frame(channelTemp = numeric(1))
  for(i in firstBin:lastBin){
    initTemps <- cbind(initTemps, data.frame(numeric(1)))
  }
  names(initTemps) <- c("channelTemp", paste0("hypoTemp", firstBin:lastBin))

  repeat{
    # RUN MODEL
    shell("cd D:/Users/sarah.fogg/Desktop/TempToolThatWorks && java -jar TempToolFour_March2015.jar", intern = internal)

    assign("cTemp", tts(odbcConnection = connect, holonName = "channel_0001", tableName = "temp_signal_output", runID = runID))

    for(z in firstBin:lastBin){
      if(z < 10){
        assign(paste0("tsz", z, "Temp"), tts(odbcConnection = odbcConnection,
                                             holonName = paste0("hyporheic_000", z),
                                             tableName = "temp_signal_output",
                                             runID = runID))
      } else {
        assign(paste0("tsz", z, "Temp"), tts(odbcConnection = odbcConnection,
                                             holonName = paste0("hyporheic_00", z),
                                             tableName = "temp_signal_output",
                                             runID = runID))
      }
    }

    # CALCULATE TEMPERATURE DIFFERENCE BETWEEN LAST AND SECOND TO LAST OUTPUT
    equilibTest <- numeric(nbins+1)
    equilibTest[1] <- cTemp$svValue[3] - cTemp$svValue[2]
    for (z in firstBin:lastBin){
      equilibTest[z - (firstBin - 2)]  <- get(paste0("tsz", z, "Temp"))$svValue[3] - get(paste0("tsz", z, "Temp"))$svValue[2]
    }

    verificationVector <- rep(0.001, times = length(equilibTest)) # all values must be less than or equal to this number
    testEquilibrium <- abs(signif(equilibTest, 1)) <= verificationVector

    # ARE ALL DIFFERENCE VALUES LESS THAN OR EQUAL TO 0.001?
    if(all(testEquilibrium) == TRUE | anyNA(testEquilibrium)){
      break
    } else {
      # RESET STARTING TEMPERATURES TO THE LAST TEMPERATURES CALCULATED (THIS WONT MATTER IF WE BREAK OUT OF THIS LOOP)
      sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = ", cTemp$svValue[3]," WHERE default_celltype.celltype = 'channel' AND default_celltype.stateval = 'Temp';"))
      for (z in firstBin:lastBin){
        if (z < 10){
          sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET Temp ='", get(paste0("tsz", z, "Temp"))$svValue[3], "' WHERE `ID`='hyporheic_000", z, "';"))
        } else {
          sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET Temp='", get(paste0("tsz", z, "Temp"))$svValue[3], "' WHERE `ID`='hyporheic_00", z, "';"))
        }
      }
    }
  }

  # PUT INIT TEMPS INTO INIT TABLE
  initTemps$channelTemp[1] <- cTemp$svValue[3]
  for(i in (firstBin+1):(lastBin+1)){
    initTemps[,i] <- get(paste0("tsz", i-1, "Temp"))$svValue[3]
  }

  return(initTemps)
}
