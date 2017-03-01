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
                           startWith){
  require(RODBC)
  require(lubridate)
  require(zoo)
  require(xts)

  nbins <- (lastBin - firstBin) + 1
  aquiferVolume <- sum(binStats[firstBin:lastBin,]$volume)

  setSkeleton(firstBin, lastBin, odbcConnection)
  setParameters(firstBin, lastBin, odbcConnection, initTemps = NULL, surfaceShade, channelSurfaceArea, channelVolume, binStats)

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

  # # NUMBER OF YEARS TO RUN TIMES THE SECONDS IN 4 YEARS DIVIDED BY THE TIME STEP
  # fourYearIntervals <- 2
  # stoppingCondition <- (fourYearIntervals * (365.25 * 4) * 86400)/timeStep
  # outputInterval <-  (365.25 * 4) * 86400/timeStep
  #
  # # SET OUTPUT INTERVAL [ONCE EVERY 4 YEARS]
  #   # CLEAR ALL ACTIVE ROWS
  #   sqlQuery(odbcConnection, "UPDATE temptoolfour.control_output SET control_output.active = 0;")
  #
  #   # IF ROWS IN THE DATAFRAME DOESN"T EXIST, CREATE IT, ELSE SET THE OUTPUT INTERVAL TO ACTIVE
  #   if(nrow(sqlQuery(odbcConnection, paste0("SELECT * FROM temptoolfour.control_output WHERE control_output.tickinterval = ", outputInterval, ";"))) == 0){
  #     sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.control_output (TableName, TickInterval, HolonType, HolonName, Currency, Behavior, StateVal, Active) VALUES ('output', '" , outputInterval,"', 'channel', '.*', '.*', '.*', 'TEMP', '1');"))
  #     sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.control_output (TableName, TickInterval, HolonType, HolonName, Currency, Behavior, StateVal, Active) VALUES ('output', '" , outputInterval,"', 'hyporheic', '.*', '.*', '.*', 'TEMP', '1');"))
  #   } else {
  #     sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.control_output SET control_output.active = 1 WHERE control_output.tickinterval = '", outputInterval,"';"))
  #   }
  #
  #   # SET STOPPING CONDITION AND TIME STEP
  #   # CLEAR ALL ACTIVE ROWS
  #   sqlQuery(odbcConnection, "UPDATE temptoolfour.control_timing SET control_timing.active = 0 ;")
  #
  #   # IF NO STOPPING CONDITION EXISTS IN DB, CREATE ROW, OTHERWISE SET THE CORRECT ROW TO ACTIVE
  #   if(nrow(sqlQuery(odbcConnection, paste("SELECT * FROM temptoolfour.control_timing WHERE control_timing.value =", stoppingCondition,";"))) == 0){
  #     sqlQuery(odbcConnection, paste0("INSERT INTO `temptoolfour`.`control_timing` (`Key`, `Value`, `Active`) VALUES ('stoppingCondition.AtTick', '", stoppingCondition, "', '1');"))
  #   } else {
  #     sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.control_timing SET control_timing.active = 1 WHERE control_timing.Key = 'stoppingCondition.AtTick' AND control_timing.Value = ", stoppingCondition, ";"))
  #   }
  #
  #   # IF NO TIME INTERVAL EXISTS IN DB, CREATE ROW, OTHERWISE SET THE CORRECT ROW TO ACTIVE
  #   if(nrow(sqlQuery(odbcConnection, paste("SELECT * FROM temptoolfour.control_timing WHERE control_timing.value =", timeStep,";"))) == 0){
  #     sqlQuery(odbcConnection, paste0("INSERT INTO `temptoolfour`.`control_timing` (`Key`, `Value`, `Active`) VALUES ('timeInterval', '", timeStep,"', '1')"))
  #   } else {
  #     sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.control_timing SET control_timing.active = 1 WHERE control_timing.Key = 'timeInterval' AND control_timing.Value = ", timeStep, ";"))
  #   }


  repeat{
    # RUN MODEL
    shell("cd D:/Users/sarah.fogg/Desktop/TempToolThatWorks && java -jar TempToolFour_March2015.jar", intern = internal)

    # QUERY TEMPERATURE VALUES FROM OUTPUT
    assign("channelInitTemp", stateValSeries(odbcConnection, "TEMP", "channel_0001", "temp_signal_output", runID = runID))
    for (z in firstBin:lastBin){
      if (z < 10){
        assign(paste0("hypo", z, "InitTemp"), stateValSeries(odbcConnection, "TEMP", holonName = paste0("hyporheic_000", z), "temp_signal_output", runID = runID))
      } else {
        assign(paste0("hypo", z, "InitTemp"), stateValSeries(odbcConnection, "TEMP", holonName = paste0("hyporheic_00", z), "temp_signal_output", runID = runID))
      }
    }

    # CALCULATE TEMPERATURE DIFFERENCE BETWEEN LAST AND SECOND TO LAST OUTPUT
    equilibTest <- numeric(nbins+1)
    equilibTest[1] <- last(channelInitTemp$svValue) - last(channelInitTemp$svValue,2)[1]
    for (z in firstBin:lastBin){
      equilibTest[z - (firstBin - 2)]  <- last(get(paste0("hypo", z, "InitTemp"))$svValue) - last(get(paste0("hypo", z, "InitTemp"))$svValue, 2)[1]
    }

    verificationVector <- rep(0.001, times = length(equilibTest)) # all values must be less than or equal to this number
    testEquilibrium <- abs(equilibTest) <= verificationVector

    # ARE ALL DIFFERENCE VALUES LESS THAN OR EQUAL TO 0.001?
    if(all(testEquilibrium) == TRUE | anyNA(testEquilibrium)){
      break
    } else {
      # RESET STARTING TEMPERATURES TO THE LAST TEMPERATURES CALCULATED (THIS WONT MATTER IF WE BREAK OUT OF THIS LOOP)
      sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = ", last(channelInitTemp$svValue)," WHERE default_celltype.celltype = 'channel' AND default_celltype.stateval = 'Temp';"))
      for (z in firstBin:lastBin){
        if (z < 10){
          sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET Temp ='", last(get(paste0("hypo", z, "InitTemp"))$svValue), "' WHERE `ID`='hyporheic_000", z, "';"))
        } else {
          sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET Temp='", last(get(paste0("hypo", z, "InitTemp"))$svValue), "' WHERE `ID`='hyporheic_00", z, "';"))
        }
      }
    }
  }

  # PUT INIT TEMPS INTO INIT TABLE
  initTemps$channelTemp[1] <- last(channelInitTemp$svValue)
  for(i in (firstBin+1):(lastBin+1)){
    initTemps[,i] <- last(get(paste0("hypo", i-1, "InitTemp"))$svValue)
  }

  return(initTemps)
}
