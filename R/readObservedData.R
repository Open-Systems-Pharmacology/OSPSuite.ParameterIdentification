#' A wrapper function around {ospsuite} functions to read an Excel-formatted
#' file as a dataset
#' @return a list of objects of class {ospsuite}::DataSet
#'
#' @param dataFolder path to data folder
#' @param dataFile filename of an Excel file
#' @param groupingColumns a vector of column names to group. No grouping added
#' if the list is NULL
#' @param sheets a list of sheet names to read. If NULL, all sheets are read
#' from file
#'
#' @examples
#' dataFolder <- file.path(getwd(), "../Data")
#' dataFile <- "DataSet.xlsx"
#' readObservedData(dataFolder, dataFile, groupingColumns = c("PK"), sheets = c("Boswell_2012"))
#'
readObservedData <- function(dataFolder, dataFile, groupingColumns = NULL, sheets = NULL) {
  if (!file.exists(file.path(dataFolder, dataFile))) {
    warning("No file found at ", file.path(dataFolder, dataFile))
    return(NULL)
  }
  dataConfiguration <- createImporterConfigurationForFile(filePath = file.path(dataFolder, dataFile))
  dataConfiguration$sheets <- sheets
  for (columnName in groupingColumns) {
    dataConfiguration$addGroupingColumn(columnName)
  }
  dataConfiguration$namingPattern <- paste0("{", paste0(groupingColumns, collapse = "}.{"), "}")
  if (dataConfiguration$namingPattern == "{}") {
    dataConfiguration$namingPattern <- "{Source}.{Sheet}"
  }
  observedData <- NULL
  try(
    observedData <- loadDataSetsFromExcel(xlsFilePath = file.path(dataFolder, dataFile), importerConfigurationOrPath = dataConfiguration)
  )
  return(observedData)
}
