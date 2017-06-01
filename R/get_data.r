library(XML)
library(reshape2)
library(plyr)
library(httr)

#' A Data Fetching Function for Population Pyramids
#'
#' This function pulls down census population data and puts it into a nice dataframe.
#' @param country Enter ISO country code (e.g. 'IZ' for Iraq)
#' @param year Enter 4-digit year (e.g. 2012)
#' @keywords census population dataframe age gender
#' @export
#' @import XML reshape2 plyr httr
#' @examples
#' get_data('IZ', 2012)

get_data <- function(country, year) {
  c1 <- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y="  
  c2 <- "&R=-1&C="
  url <- paste0(c1, year, c2, country)
  print(url)
  url <- GET(url)
  df <- data.frame(readHTMLTable(rawToChar(url$content),stringsAsFactors=F))
  keep <- c(2, 4, 5)
  df <- df[,keep]  
  names(df) <- c("Age", "Male", "Female")
  cols <- 2:3
  df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))
  df <- df[df$Age != 'Total', ]  
  df$Male <- -1 * df$Male
  df$Age <- factor(df$Age, levels = df$Age, labels = df$Age)
  
  df.melt <- melt(df, 
                  value.name='Population', 
                  variable.name = 'Gender', 
                  id.vars='Age' )
  
  return(df.melt)
}