#' Set model timing.
#'
#' @param odbcConnection RODBC object.
#' @param timeStep model time step in seconds. numeric.
#' @param yearsToRun how long to run model, in years. numeric.
#' @param outputInterval model will output at this time, in seconds. numeric.
#'
#' @return Does not return anything, check model database to see changes.


setTiming <- function(odbcConnection, timeStep, yearsToRun, outputInterval){

  outputTickInterval <- outputInterval/timeStep # Default is to output once an hour
  stoppingCondition <- (yearsToRun * 86400 * 365.25)/timeStep

  # CLEAR control_output
  sqlQuery(odbcConnection, "UPDATE temptoolfour.control_output SET control_output.active = 0;")

  # SET control_output.tickinterval
  if(nrow(sqlQuery(odbcConnection, paste0("SELECT * FROM temptoolfour.control_output WHERE control_output.tickinterval = ", outputTickInterval, ";"))) == 0){
    sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.control_output (TableName, TickInterval, HolonType, HolonName, Currency, Behavior, StateVal, Active) VALUES ('output', '" , outputTickInterval,"', 'channel', '.*', '.*', '.*', 'TEMP', '1');"))
    sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.control_output (TableName, TickInterval, HolonType, HolonName, Currency, Behavior, StateVal, Active) VALUES ('output', '" , outputTickInterval,"', 'hyporheic', '.*', '.*', '.*', 'TEMP', '1');"))
  } else {
    sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.control_output SET control_output.active = 1 WHERE control_output.tickinterval = '", outputTickInterval, "' AND control_output.HolonType = 'channel';"))
    sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.control_output SET control_output.active = 1 WHERE control_output.tickinterval = '", outputTickInterval, "' AND control_output.HolonType = 'hyporheic';"))
  }

  # SET STOPPING CONDITION AND TIME STEP
  # CLEAR ALL ACTIVE ROWS
  sqlQuery(odbcConnection, "UPDATE temptoolfour.control_timing SET control_timing.active = 0 ;")

  # IF NO STOPPING CONDITION EXISTS IN DB, CREATE ROW, OTHERWISE SET THE CORRECT ROW TO ACTIVE
  if(nrow(sqlQuery(odbcConnection, paste("SELECT * FROM temptoolfour.control_timing WHERE control_timing.value =", stoppingCondition,";"))) == 0){
    sqlQuery(odbcConnection, paste0("INSERT INTO `temptoolfour`.`control_timing` (`Key`, `Value`, `Active`) VALUES ('stoppingCondition.AtTick', '", stoppingCondition, "', '1');"))
  } else {
    sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.control_timing SET control_timing.active = 1 WHERE control_timing.Key = 'stoppingCondition.AtTick' AND control_timing.Value = ", stoppingCondition, ";"))
  }

  # IF NO TIME INTERVAL EXISTS IN DB, CREATE ROW, OTHERWISE SET THE CORRECT ROW TO ACTIVE
  if(nrow(sqlQuery(odbcConnection, paste("SELECT * FROM temptoolfour.control_timing WHERE control_timing.value =", timeStep,";"))) == 0){
    sqlQuery(odbcConnection, paste0("INSERT INTO `temptoolfour`.`control_timing` (`Key`, `Value`, `Active`) VALUES ('timeInterval', '", timeStep,"', '1')"))
  } else {
    sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.control_timing SET control_timing.active = 1 WHERE control_timing.Key = 'timeInterval' AND control_timing.Value = ", timeStep, ";"))
  }
}
