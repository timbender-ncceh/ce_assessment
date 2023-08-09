valid_ssn <- function(ssn){
  # checks whether an input ssn value (numeric or string) is valid based on SSA
  # and USPS rules for issuing Social Security Numbers. See: SSA/USPS website
  # for more information.  Created in 2022
  ssn <- as.character(ssn)
  if(nchar(ssn) < 9){
    ssn <- paste(rep("0", 9-length(ssn)), 
                 ssn, sep = "", collapse = "")
  }
  
  area_digits    <- substr(ssn, 1,3)
  group_digits   <- substr(ssn, 4,5)
  serial_digits    <- substr(ssn, 6,9)
  
  invalid.area   <- c(0, 666, 900:999)
  invalid.group   <- c(0)
  invalid.serial <- c(0)
  
  return(!(any(as.numeric(area_digits) %in% invalid.area | 
                 as.numeric(group_digits) %in% invalid.group | 
                 as.numeric(serial_digits) %in% invalid.serial)))
}
