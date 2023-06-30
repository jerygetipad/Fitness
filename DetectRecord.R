detect_record <- function(data=fitness, exercise_list) {
  records <- 0
  for(e in exercise_list) {
    ex_data <- data |> 
      filter(exercise == e) |>
      filter(complete == "Y") 
    # If all weights are NA, skip
    if((ex_data[,5:9] |> is.na() |> all())) {
      # Print to Log only
      paste0("skip ",e) |>
        write("./Logs/tmplog.txt",append = T)
      # 
      next
    }
    # Add Max Weight Column
    ex_data <- ex_data |>
      rowwise() |>
      mutate(maxweight = max(repweight, w1, w2, w3, w4, w5, na.rm=TRUE))
    
    max_row <- ex_data[which.max(ex_data$maxweight),] |>
      mutate(
        exercise = str_to_title(exercise)
      )
    # Grab Latest Day
    last_day <- data |> 
      filter(week==max(week), session==max(session)) |>
      select(week,session) |> 
      unique() |>
      unlist()
    
    if(max_row$week==last_day[1] & max_row$session == last_day[2]) {
      # Write Record to Report
      records = records + 1
      print(glue::glue("NEW PERSONAL RECORD: {max_row$exercise} - {max_row$maxweight} lbs."))
    }
  }
  if(records==0) {
    print("No Personal Records set")
  }
}
