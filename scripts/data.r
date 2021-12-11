# Data

library(rio)

get_data <- function() {  
  d <- import("../data_clean/wyre.csv")
  return(d)
}