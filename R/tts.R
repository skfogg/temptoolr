tts = function(odbcConnection, holonName, tableName, runID = "", xtsIndex = NULL) {

  newTemperatureSeries <- structure(
      list(stateVal = "TEMP", holon = holonName, table = tableName, runID = runID),
      class = "TemperatureSeries"
    )

  sqlStmt <- paste0("SELECT modelTime, svValue FROM ", tableName, " WHERE holonname = '", holonName, "' AND stateVal = 'TEMP'")
  queriedTemp <- sqlQuery(odbcConnection, sqlStmt)

  nReturned <- nrow(queriedTemp)
  if(nReturned == 0 ) stop("No records found for: ", paste0(names(newTemperatureSeries), "='", newTemperatureSeries, "'", collapse = ", "))

  if (!is.null(xtsIndex)){
    queriedTemp <- xts(zoo(queriedTemp, order.by = xtsIndex))
  }

  tts <- structure(
    queriedTemp,
    class = c("tts", class(queriedTemp)),
    spec = newTemperatureSeries,
    queryDate = date(),
    runID = runID
  )

  return(tts)
}

plot.tts = function(x, ...) {
  if(is.xts(x)) {
    plot(x = as.zoo.default(x$svValue), ...)
  } else {
    plot(x = x$modelTime, y = x$svValue, ...)
  }
}

lines.tts = function(x, ...) {
  if(is.xts(x)) {
    lines(x = as.zoo(x$svValue), ...)
  } else {
    lines(x = x$modelTime, y = x$svValue, ...)
  }
}

points.tts = function(x, ...) {
  if(is.xts(x)) {
    points(x = as.zoo(x$svValue), ...)
  } else {
    points(x = x$modelTime, y = x$svValue, ...)
  }
}

print.TemperatureSeries = function(x, ...) {
  print(paste0(names(x), " = '", x, "'", collapse = "; "), ...)
}
