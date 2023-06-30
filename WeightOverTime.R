weight_over_time_v1 <- function(exercise) {
  # Filter Data
  ex_data = fitness |> 
    #filter(exercise == .data$exercise) |>
    filter(exercise == "Incline Bench") |>
    select(-c(duration, notes, repweight)) |>
    pivot_longer(
      cols=c(w1,w2,w3,w4,w5), 
      names_to = "rep",
      values_to = "weight") |>
    mutate(
      rep = str_sub(rep,-1)
    ) 
 ex_data <- bind_rows(
    ex_data |> mutate(rep = as.character(rep)),
    ex_data |> group_by(week,session) %>% 
      summarize(weight=mean(weight)) %>% 
      mutate(rep = "avg.")
  ) |>
    arrange(week,rep)
  # Create Title
  plot_title = glue::glue("Ben Gramza - {exercise} weight over time") |>
    str_to_title()
  # Create Plot
  ggplot(data=ex_data,aes(x=week, y=weight)) + 
    #geom_point() + 
    theme_bw() + 
    scale_x_discrete(limits=1:2) +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth",
                se=FALSE) + 
    # Add Rep Labels
    ggrepel::geom_label_repel(
      aes(label=rep), 
      direction="x", 
      box.padding = 0, 
      label.padding = .2,
      point.size = NA,
      color="black") +
    # Write Title
    labs(
      x = "Week",
      y = "Weight (in lbs.)",
      title = plot_title, 
      subtitle = "w/ Rep Labels and Regression Line"
    )
}
weight_over_time_v2 <- function(weighted_exercises) {
  # Filter Data
  ex_data = fitness |> 
    #filter(exercise == .data$exercise) |>
    filter(exercise %in% weighted_exercises)|>
    select(-c(duration, notes, repweight)) |>
    pivot_longer(
      cols=c(w1,w2,w3,w4,w5), 
      names_to = "rep",
      values_to = "weight") |>
    group_by(week,session,exercise) |>
      summarize(ave=mean(weight, na.rm=TRUE),
                max = max(weight, na.rm=TRUE)) |>
    pivot_longer(
      cols=c(ave,max),
      names_to = "type",
      values_to = "weight"
    ) |>
    mutate(
      weight = round(weight,digits=0),
    )
  # Create Plot
  ggplot(data=ex_data,aes(x=week,y=weight,color=type)) + 
    geom_point() + 
    theme_bw() + 
    scale_x_discrete(limits=1:max(ex_data$week)) +
    stat_smooth(method = "loess",
                formula = y ~ x,
                geom = "smooth",
                se=FALSE) +
    #expand_limits(y=0) + 
    #scale_y_continuous(expand = c(0, 0)) + 
    facet_wrap(~exercise, scales= "free", nrow = 5, ncol=3) +
    # Write Title
    labs(
      x = "Week",
      y = "Weight (in lbs.)",
      title = "Weight over Time By Exercise", 
      subtitle = "w/ Regression Lines",
      color = "Metric"
    ) + 
    #theme(aspect.ratio = 1) +
    scale_y_continuous(labels = function(x) sprintf("%0.0f", x)) +
    scale_color_discrete(name = "Metric", labels = c("Mean (lbs./rep)", "Max Weight")) +
    theme(legend.position = "bottom",
            legend.box = "vertical") 

}
