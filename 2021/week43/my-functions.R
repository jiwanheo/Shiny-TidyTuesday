produce_summary <- function(df) {
  df %>% 
    group_by(State = state, Type = type) %>% 
    summarize(Min = round(summary(weight)[1], digits = 2),
              `1st Qu.` = round(summary(weight)[2], digits = 2),
              Median = round(summary(weight)[3], digits = 2),
              Mean = round(summary(weight)[4], digits = 2),
              `3rd Qu.` = round(summary(weight)[5], digits = 2),
              Max = round(summary(weight)[6], digits = 2),
              .groups = "drop"
    )
}