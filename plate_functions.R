
# functions for various lab tasks
library(stringr)
library("dplyr")
library("tidyr")

# function to vectorize 96 well or 384 well plates
vectorize.plate<-function(df, wellName = "plateIndex", valueName = "values", plate = 96) {
  x <- as.matrix(t(df))
  values <- as.vector(unlist(x))
  if (plate == 96) {
    plateIndex <- unlist(lapply(LETTERS[1:8], function(x) {paste(x,1:12, sep = "")}))
  } else if (plate == 384) {
    plateIndex <- unlist(lapply(LETTERS[1:16], function(x) {paste(x,1:24, sep = "")}))
  }
  longplate <- data.frame(plateIndex, values)
  colnames(longplate) <- c(wellName, valueName)
  return(longplate)
}

# function that takes a the output of vectorize 384 and returns their corresponding 96 well id and quadrant number
translate.384to96<-function(well384positions, wellCol = "Well", wellZero = TRUE) {
  inputPlate <- well384positions %>% 
    rename(Well384 = wellCol)
  wells96 <- unlist(lapply(LETTERS[1:8], function(x) {paste0(x, 1:12)}))
  if (nchar(inputPlate[1]) == 3 || wellZero) {
    oddQcol <- formatC(seq(1,24,2), width = 2, flag = 0)
    evenQcol <- formatC(seq(2,24,2), width = 2, flag = 0)
  } else {
    oddQcol <- seq(1,24,2)
    evenQcol <- seq(2,24,2)
  }
  plateIndex <- unlist(lapply(LETTERS[1:8], function(x) {paste(x,1:12, sep = "")}))
  # loops to create quadrant indexes
  oddQrow <- LETTERS[seq(1,16,2)]
  evenQrow <- LETTERS[seq(2,16,2)]
  dfList <- data.frame(Well384 = unlist(lapply(oddQrow, function(x) {paste(x, oddQcol, sep = "")})), 
                       well96 = wells96, quadrant = 1, hRep = 1, vRep = 1)
  dfList <- rbind(dfList, data.frame(Well384 = unlist(lapply(oddQrow, function(x) {paste(x, evenQcol, sep = "")})), 
                                     well96 = wells96, quadrant = 2, hRep = 2, vRep = 1))
  dfList <- rbind(dfList, data.frame(Well384 = unlist(lapply(evenQrow, function(x) {paste(x, oddQcol, sep = "")})), 
                                     well96 = wells96, quadrant = 3, hRep = 1, vRep = 2))
  dfList <- rbind(dfList, data.frame(Well384 = unlist(lapply(evenQrow, function(x) {paste(x, evenQcol, sep = "")})), 
                                     well96 = wells96, quadrant = 4, hRep = 2, vRep = 2))
  
  # gets 96 well ids
  matchdf <- merge(inputPlate, dfList, by = "Well384", sort = F)
  return(matchdf)
}
