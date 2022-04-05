#' Plot rh reserve times
plot_reserve_times <- function(reserve_times){
  plot <- reserve_times %>% 
  pivot_longer(-1, "scenario") %>% 
  ggplot(aes(x = scenario, y = value, fill = Outcome)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value,2)),
            position = position_dodge(width = 0.9),
            vjust = - 0.2) +
  theme_minimal() +
  labs(title = "Median time between rh reservation and outcome", 
       y = "Time (min)",
       x = "Scenario")
  
  ggsave(paste0("output/plots/reserve_times.png"),
         plot,
         width = 8, height = 4, units = "in",
         device = ragg::agg_png, scaling = 1,
         bg = "white")
  
  plot
}


#' plots rh travel times for each scenario by zone
plot_travel_times <- function(events_list){
  for(i in 1:length(events_list)){
    plot <- rh_travel_times(events_list[[i]]) %>% 
      {ggplot(., aes(x = area, y = travelTime)) +
          geom_boxplot(na.rm = T) +
          labs(x = "Zone",
               y = "Travel time (min)",
               title = paste("Travel times in",
                             names(events_list)[i],
                             "scenario"))
      }
    print(plot)
    ggsave(paste0("output/plots/travel_times_", names(events_list)[i], ".png"),
           plot,
           width = 8, height = 4, units = "in",
           device = ragg::agg_png, scaling = 1,
           bg = "white")
  }  
}