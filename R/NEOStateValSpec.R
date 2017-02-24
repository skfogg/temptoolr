stateValSeries = function(connect, stateValName, holonName, tableName, runID = "", xtsIndex = NULL) {
  newStateValSpec =
    structure(
      list(stateVal = stateValName, holon = holonName, table = tableName, runID = runID),
      class = "NEOStateValSpec"
    )
  sqlStmt = paste0("SELECT modelTime, svValue FROM ", tableName, " WHERE holonname = '", holonName, "' AND stateVal = '", stateValName, "'")
  sVSeries <- sqlQuery(connect, sqlStmt)
  nReturned = nrow(sVSeries)
  if(nReturned == 0 ) stop("No records found for: ", paste0(names(newStateValSpec), "='", newStateValSpec, "'", collapse = ", "))
  if (!is.null(xtsIndex)){
    sVSeries = xts(zoo(sVSeries, order.by = xtsIndex))
  }
  sVSeries =
    structure(
      sVSeries,
      class = c("NEOStateValSeries", class(sVSeries)),
      spec = newStateValSpec
    )
  return(sVSeries)
}

plot.NEOStateValSeries = function(x, ...) {
  if(is.xts(x)) {
    plot(x = as.xts(x$svValue), ...)
  } else {
    plot(x = x$modelTime, y = x$svValue, ...)
  }
}

lines.NEOStateValSeries = function(x, ...) {
  if(is.xts(x)) {
    lines(x = as.xts(x$svValue), ...)
  } else {
    lines(x = x$modelTime, y = x$svValue, ...)
  }
}

points.NEOStateValSeries = function(x, ...) {
  if(is.xts(x)) {
    points(x = as.xts(x$svValue), ...)
  } else {
    points(x = x$modelTime, y = x$svValue, ...)
  }
}


print.NEOStateValSpec = function(x, ...) {
  print(paste0(names(x), " = '", x, "'", collapse = "; "), ...)
}
