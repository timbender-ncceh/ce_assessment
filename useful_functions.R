
# Math-----

# convert decimal numbers to any other baseN number
dec2baseN <- function(decimal, baseN, separator = "."){
  out <- NULL
  if(decimal > 1){
    out <- c(out, dec2baseN(as.integer(decimal/baseN),baseN))
  }
  out <- paste(c(out, decimal %% baseN),sep = separator,collapse = separator)
  return(out)
}


# Dates----
