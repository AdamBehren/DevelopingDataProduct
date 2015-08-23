getDatePointer <- function(inputDate, sp_data){
  for (i in 1:7) {
    x <- match(inputDate, sp_data$Date)
    if (is.na(x)) {inputDate <- inputDate - 1}
    else break
  }
  return(x)
}


