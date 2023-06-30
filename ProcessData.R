# Process Data
library(tidyverse)
# Input weekly files, output combined dataset
process_data <- function() {
  files <- list.files("./Data/", "Week*", full.names = T)
  
  # Initialize masterdf
  s.df <- data.frame()
  
  for(i in 1:length(files)) {
    # Get Sheet Names
    sheets <- readxl::excel_sheets(files[i])
    # Initialize List
    s <- list()
    # 
    tmp <- readxl::read_xlsx(files[i], sheet = sheets[1])
    # Check if any weights are not NA
    if(!(tmp[,4:9] |> is.na() |> all())){
      tmp$week <- i
      tmp$session <- 1
      s[[1]] <- tmp
      
    }
    tmp <- readxl::read_xlsx(files[i], sheet = sheets[2])
    if(!(tmp[,4:9] |> is.na() |> all())){
      tmp$week <- i
      tmp$session <- 2
      s[[2]] <- tmp
    }
    tmp <- readxl::read_xlsx(files[i], sheet = sheets[3])
    if(!(tmp[,4:9] |> is.na() |> all())){
      tmp$week <- i
      tmp$session <- 3
      s[[3]] <- tmp
    }
    # Add Week and Session #'s
    
    # Combine List of Workout Sheets into a Dataframe object
    s.df <- rbind(
      s.df,
      data.table::rbindlist(s, fill=TRUE)
    )
  }
  
  # Save Data.Frame
  write_csv(x=s.df, file = "./Data/Master.csv")
  
  # Write this to log
  message("Data Processed")
}
