#' Set the model parameters.
#'
#' @param firstBin numeric. The smallest TSZ in model run.
#' @param lastBin numeric. The largest TSZ in model run.
#' @param odbcConnection RODBC object.
#' @param initTemps dataframe of starting temperatures. The default is initTemps = NULL, which starts all model cells at 1 degree C.
#' @param surfaceShade numeric. Proportion of channel under shade.
#' @param channelSurfaceArea numeric. (m2)
#' @param channelVolume numeric. (m3)
#' @param binStats dataframe from hyporheicBins(). Geometry of Hyporheic Zone.
#'
#' @return Does not return anything, check model database to see changes.
#' @export


setParameters <- function(firstBin, lastBin, odbcConnection, initTemps = NULL, surfaceShade, channelSurfaceArea, channelVolume, binStats){

  nbins <- (lastBin - firstBin) + 1
  aquiferVolume <- sum(binStats[firstBin:lastBin,]$volume)

  if(is.null(initTemps)){
    # SET ALL TEMPERATURES TO 1 DEG C
    sqlQuery(odbcConnection, "UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = 1 WHERE default_celltype.celltype = 'channel' AND default_celltype.stateval = 'Temp';")
    sqlQuery(odbcConnection, "UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = 1 WHERE default_celltype.celltype = 'hyporheic' AND default_celltype.stateval = 'Temp';")
    sqlQuery(odbcConnection, "UPDATE temptoolfour.init_heat_cell_hyporheic SET init_heat_cell_hyporheic.Temp = 1;")
  } else {
    # INITIALIZE HYPORHEIC SUB-ZONE TEMPERATURES
    for (z in firstBin:lastBin){
      if (z < 10){
        sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET Temp ='", initTemps[, colnames(initTemps) == paste0("hypoTemp", z)], "' WHERE `ID`='hyporheic_000", z, "';"))
      } else {
        sqlQuery(connect, paste0("UPDATE `temptoolfour`.`init_heat_cell_hyporheic` SET `Temp`='", initTemps[, colnames(initTemps) == paste0("hypoTemp", z)], "' WHERE `ID`='hyporheic_00", z, "';"))
      }
    }

    # INITIALIZE CHANNEL TEMPERATURE
    sqlQuery(connect, paste0("UPDATE temptoolfour.default_celltype SET default_celltype.DefaultVal = '", initTemps[, colnames(initTemps) == "channelTemp"], " '  WHERE default_celltype.CellType = 'channel' AND default_celltype.StateVal = 'Temp';"))
  }


  # UPDATE SURFACE SHADE
  sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.ts_shade SET ts_shade.value = ", surfaceShade, " WHERE ts_shade.key = 1;"))
  sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.ts_shade SET ts_shade.value = ", surfaceShade, " WHERE ts_shade.key = 1e200;"))

  # UPDATE CHANNEL SURFACE AREA
  sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = '", channelSurfaceArea, "' WHERE default_celltype.celltype='channel' And default_celltype.stateval='Area';"))

  # UPDATE CHANNEL VOLUME
  sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = '", channelVolume, "' WHERE default_celltype.celltype='channel' And default_celltype.stateval='Water';"))

  # UPDATE CHANNEL SUBSURFACE AREA
  sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = '", channelSurfaceArea, "' WHERE default_celltype.celltype='hyporheic' And default_celltype.stateval='Area';"))

  # UPDATE HYPORHEIC VOLUME AKA "AQUIFER VOLUME"
  sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = '", aquiferVolume, "' WHERE default_celltype.celltype ='hyporheic' And default_celltype.stateval ='Volume';"))

  # UPDATE HYPORHEIC SUBZONE VOLUMES
  for (z in firstBin:lastBin){
    if (z < 10){
      sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET HypoVolume='", binStats$volume[z], "' WHERE ID = 'hyporheic_000", z, "';"))
    } else {
      sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET HypoVolume='", binStats$volume[z], "' WHERE ID = 'hyporheic_00", z, "';"))
    }
  }

  # UPDATE GROSS HYPORHEIC INFLOW
  if(firstBin < 10){
    sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_edge_gwflow SET InFlow ='", sum(binStats[firstBin:lastBin,]$returnFlow), "' WHERE ID = 'bedto_000", firstBin,"';"))
  } else {
    sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_edge_gwflow SET InFlow ='", sum(binStats[firstBin:lastBin,]$returnFlow), "' WHERE ID = 'bedto_00", firstBin,"';"))
  }

  # UPDATE INTERZONE INFLOWS
  for (z in (firstBin+1):lastBin){
    if (z < 10){
      sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_edge_gwflow_interzone SET InFlow ='", binStats$inFlow[z], "' WHERE ID = 'bedto_000", z, "';"))
    } else {
      sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_edge_gwflow_interzone SET InFlow ='", binStats$inFlow[z], "' WHERE ID = 'bedto_00", z, "';"))
    }
  }

  # UPDATE RETURN FLOWS
  for (z in firstBin:lastBin){
    if (z < 10){
      sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_edge_gwflow_discharge SET OutFlow = '", binStats$returnFlow[z], "' WHERE ID = 'bedfrom_000", z, "';"))
    } else {
      sqlQuery(odbcConnection, paste0("UPDATE temptoolfour.init_heat_edge_gwflow_discharge SET OutFlow = '", binStats$returnFlow[z], "' WHERE ID ='bedfrom_00", z, "';"))
    }
  }

}
