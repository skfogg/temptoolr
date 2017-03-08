#' Set the model configuration tables.
#'
#' @param firstBin numeric, the smallest TSZ considered in model run
#' @param lastBin numeric, the largest TSZ in model run
#' @param odbcConnection RODBC object
#'
#' @return Does not return anything, check model database to see changes.
#' @export

setSkeleton <- function(firstBin, lastBin, odbcConnection){

  #### MODEL CONFIGURATION TABLES ####
  ## matrix_cell ##
  # DELETE ALL hyporheic CELLS #
  sqlQuery(odbcConnection, "DELETE FROM `temptoolfour`.`matrix_cell` WHERE `CellType`='hyporheic';")

  # INSERT hyporheic BINS FOR THIS MODEL #
  for(z in firstBin:lastBin){
    if(z < 10){
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_cell (ID, CellType) VALUES ('hyporheic_000", z,"', 'hyporheic');"))
    } else {
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_cell (ID, CellType) VALUES ('hyporheic_00", z,"', 'hyporheic');"))
    }
  }

  ## matrix_edge ##
  # DELETE ALL gwflow% EDGES #
  sqlQuery(odbcConnection, "DELETE FROM temptoolfour.matrix_edge WHERE matrix_edge.EdgeType LIKE 'gwflow%';")

  # INSERT gwflowback EDGES #
  for(z in firstBin:lastBin){
    if(z < 10){
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_edge (ID, EdgeType, FromCell, ToCell) VALUES ('bedfrom_000", z,"', 'gwflowback', 'hyporheic_000", z,"', 'channel_0001');"))
    } else {
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_edge (ID, EdgeType, FromCell, ToCell) VALUES ('bedfrom_00", z,"', 'gwflowback', 'hyporheic_00", z,"', 'channel_0001');"))
    }
  }

  # INSERT gwflow EDGE #
  if(firstBin < 10){
    sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_edge (ID, EdgeType, FromCell, ToCell) VALUES ('bedto_000", firstBin,"', 'gwflow', 'channel_0001', 'hyporheic_000", firstBin,"');"))
  } else {
    sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_edge (ID, EdgeType, FromCell, ToCell) VALUES ('bedto_00", firstBin,"', 'gwflow', 'channel_0001', 'hyporheic_00", firstBin,"');"))
  }

  # INSERT gwflowbetween EDGES #
  for(z in (firstBin+1):lastBin){
    if(z < 10){
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_edge (ID, EdgeType, FromCell, ToCell) VALUES ('bedto_000", z,"', 'gwflowbetween', 'hyporheic_000", z-1,"', 'hyporheic_000", z,"')"))
    }
    if (z == 10){
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_edge (ID, EdgeType, FromCell, ToCell) VALUES ('bedto_00", z,"', 'gwflowbetween', 'hyporheic_000", z-1,"', 'hyporheic_00", z,"')"))
    }
    if(z > 10){
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.matrix_edge (ID, EdgeType, FromCell, ToCell) VALUES ('bedto_00", z,"', 'gwflowbetween', 'hyporheic_00", z-1,"', 'hyporheic_00", z,"')"))
    }
  }

  ## init_heat_cell_hyporheic ##
  # DELETE ALL #
  sqlQuery(odbcConnection, "DELETE FROM temptoolfour.init_heat_cell_hyporheic;")

  # INSERT hyporheic_00% INIT CELLS #
  for(z in firstBin:lastBin){
    if(z < 10){
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.init_heat_cell_hyporheic (ID, Temp, HypoVolume) VALUES ('hyporheic_000", z,"', '1', '1');"))
    } else {
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.init_heat_cell_hyporheic (ID, Temp, HypoVolume) VALUES ('hyporheic_00", z,"', '1', '1');"))
    }
  }

  ## init_heat_edge_gwflow ##
  # DELETE #
  sqlQuery(odbcConnection, "DELETE FROM temptoolfour.init_heat_edge_gwflow;")

  # INSERT bedto_00% CELL #
  if(firstBin < 10){
    sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.init_heat_edge_gwflow (ID, Inflow) VALUES ('bedto_000", firstBin,"', '1')"))
  } else{
    sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.init_heat_edge_gwflow (ID, Inflow) VALUES ('bedto_00", firstBin,"', '1')"))
  }

  ## init_heat_edge_gwflow_discharge ##
  # DELETE bedfrom_00% #
  sqlQuery(odbcConnection, "DELETE FROM temptoolfour.init_heat_edge_gwflow_discharge;")

  # INSERT bedfrom_00% EDGES #
  for(z in firstBin:lastBin){
    if(z < 10){
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.init_heat_edge_gwflow_discharge (ID, OutFlow) VALUES ('bedfrom_000", z,"', '1');"))
    } else {
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.init_heat_edge_gwflow_discharge (ID, OutFlow) VALUES ('bedfrom_00", z,"', '1');"))
    }
  }

  ## init_heat_edge_gwflow_interzone ##
  # DELETE INTERZONE FLOWS #
  sqlQuery(odbcConnection, "DELETE FROM temptoolfour.init_heat_edge_gwflow_interzone;")

  # INSERT INTERZONE bedto_00% CELLS #
  for(z in (firstBin+1):lastBin){
    if(z < 10){
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.init_heat_edge_gwflow_interzone (ID, Inflow) VALUES ('bedto_000", z,"', '1');"))
    } else {
      sqlQuery(odbcConnection, paste0("INSERT INTO temptoolfour.init_heat_edge_gwflow_interzone (ID, Inflow) VALUES ('bedto_00", z,"', '1');"))
    }
  }

}
