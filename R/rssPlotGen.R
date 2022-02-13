# Retrieves the publishing dates of articles in an RSS feed.

require(XML)
require(RCurl)
require(stringr)

# Generates a plot of the RSS publishing data.
genPlot <- function() {

  Sys.setenv("LANGUAGE"="EN")
  itemList <- genItemList()
  tableData <- formatValues(itemList)

  par(mfrow = c(2, 1), mar = c(2, 4, 4, 4))
  barplot(tableData, las = 2, main="Blogging Data")
}


# Generates a list of RSS blog entry items from the URL.
genItemList <- function() {

  # Insert the URL of the RSS Feed
  fileURL <- "https://denisthiessen.de/feed.xml"

  xData <- getURL(fileURL, ssl.verifypeer = FALSE)
  data <- xmlParse(xData)
  root <- xmlRoot(data)
  itemList <- xmlToList(root[[1]])

  return(itemList)
}


# Generates a table with the dates from the XML RSS items.
formatValues <- function(itemList) {

  pubDates <- c()
  result <- c()

  # Extract the raw publishing dates.
  for(i in itemList) {
    pubDates <- append(toString(i[names(i) == "pubDate"]), pubDates)
  }

  pubDates <- pubDates[! pubDates %in% c("")]

  # Then format them to a table format.
  for(pubDate in pubDates) {
    pubSplit <- str_split(pubDate, ",")
    pubSplitValue <- pubSplit[[1]]

    if (grepl("-", pubSplitValue[2], fixed=TRUE)){
      extractedValue <- str_split(pubSplitValue[2], "-")
    }
    else {
      extractedValue <- str_split(pubSplitValue[2], "\\+")
    }

    result <- append(extractedValue[[1]][[1]], result)
  }

  return(table(result))
}
