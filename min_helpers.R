#helper functions for minor data wrangling or data processing tasks


#===================
#rounding functions
#===================
round2 <- function(x){
  y <- janitor::round_half_up(x, 2)
  return(y)
}

round3 <- function(x){
  y <- janitor::round_half_up(x, 3)
  return(y)
}



#===================
#calculate NA percentage
#===================
#this function calculates the percentage of missing data for all variables in df
quantify_nas <- function(data){
  
  
  output_table <- data.frame()
  
  for (c in 1:ncol(data)){
    
    temp_output_table <- data.frame(column = NA,
                                    missing_pct = NA)
    temp_output_table$column[1] <- colnames(data[,..c])
    temp_output_table$missing_pct[1] <- janitor::round_half_up(naniar::pct_miss(data[,..c]), 2)
    
    output_table <- rbind(output_table, temp_output_table)
    
  }
  return(output_table)
}