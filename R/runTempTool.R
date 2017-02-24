runTempTool <- function(odbcConnection,
                         binStats,
                         initTemps = NULL,
                         channelVolume,
                         channelSurfaceArea,
                         surfaceShade,
                         firstBin,
                         lastBin,
                         timeStep,
                         yearsToRun,
                         outputInterval,
                         runID,
                         outputIndex = NULL,
                         internal = TRUE){

  require(RODBC)
  require(lubridate)
  require(zoo)
  require(xts)
  require(hydrogeom)

  setSkeleton(firstBin, lastBin, odbcConnection)
  setParameters(firstBin, lastBin, odbcConnection, initTemps, surfaceShade, channelSurfaceArea, channelVolume, binStats)
  setTiming(odbcConnection, timeStep, yearsToRun, outputInterval)

  ### RUN MODEL ###
  shell("cd D:\\Users\\sarah.fogg\\Desktop\\TempToolThatWorks && java -jar TempToolFour_March2015.jar", intern=TRUE)

  connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="password")

  # QUERY ALL TEMPERATURE OUTPUT
  assign(paste0("channelTemp_run", runID), stateValSeries(connect, "TEMP", "channel_0001", "temp_signal_output", runID = runID, xtsIndex = outputIndex))

  for(z in firstBin:lastBin){
    if(z < 10){
      assign(paste0("hypo", z,"Temp_run", runID), stateValSeries(connect, "TEMP", paste0("hyporheic_000", z), "temp_signal_output", runID = runID, xtsIndex = outputIndex))
    } else {
      assign(paste0("hypo", z,"Temp_run", runID), stateValSeries(connect, "TEMP", paste0("hyporheic_00", z), "temp_signal_output", runID = runID, xtsIndex = outputIndex))
    }
  }


  # CREATE LIST OF ALL RUN DATA
  objectNames <- c(paste0("channelTemp_run", runID), paste0("hypo", firstBin:lastBin, "Temp_run", runID))
  # assign(paste0("output_run", runID), lapply(objectNames, get))

  outList <- as.list(objectNames)

  for(i in 1:length(objectNames)){
    outList[[i]] <- get(objectNames[i])
  }


  return(outList)

  }
