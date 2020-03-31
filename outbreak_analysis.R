# LIBRARIES & CONSTANTS ----

library("tidyverse")
library("gganimate")
library("gifski")
#devtools::install_github("thomasp85/transformr")
library("transformr")
library("scales")
library("ggrepel")


# PULL DATA ----

outbreak_data <- 
  readxl::read_xlsx(path = "outbreak_data.xlsx",
                    sheet = 'covid19_2019') %>% 
  select(date, 
         deaths) %>%
  mutate(outbreak = 'COVID-19 (2019)') %>% 
  
  #bind_rows(
  #  
  #  readxl::read_xlsx(path = "~/Desktop/outbreak_data.xlsx",
  #                    sheet = 'cholera_yemen_2017') %>% 
  #    select(date, deaths) %>%
  #    mutate(outbreak = 'Cholera - Yemen (2019)')
  #  
  #) %>% 
  
  bind_rows(
    
    readxl::read_xlsx(path = "outbreak_data.xlsx",
                      sheet = 'sars_2003') %>% 
      select(date, 
             deaths) %>%
      mutate(outbreak = 'SARS (2003)')
    
  ) %>% 

  
  bind_rows(
    
    readxl::read_xlsx(path = "outbreak_data.xlsx",
                      sheet = 'ebola_2014') %>% 
      select(date, 
             deaths) %>%
      mutate(outbreak = 'Ebola (2014)')
    
  ) %>% 
  
  bind_rows(
    
    readxl::read_xlsx(path = "outbreak_data.xlsx",
                      sheet = 'cholera_haiti_2010') %>% 
      select(date, 
             deaths) %>%
      mutate(outbreak = 'Cholera - Haiti (2014)')
    
  ) %>% 
  
  bind_rows(
    
    readxl::read_xlsx(path = "outbreak_data.xlsx",
                      sheet = 'mers_2012') %>% 
      select(date, 
             deaths) %>%
      mutate(outbreak = 'MERS (2012)')
    
  ) %>% 
  
  bind_rows(
    
    readxl::read_xlsx(path = "outbreak_data.xlsx",
                      sheet = 'h1n1_swine_2009') %>% 
      select(date, 
             deaths) %>%
      mutate(outbreak = 'H1N1 Swine Flu (2009)')
    
  ) %>% 
  
  group_by(outbreak) %>% 
  mutate(min_date = min(date)) %>% 
  ungroup() %>% 
  mutate(days = as.numeric(difftime(as.POSIXct(date), 
                                    as.POSIXct(min_date, tz="UTC"), 
                                    units="days"))) %>% 
  select(outbreak, 
         days, 
         deaths) %>%
  arrange(outbreak, 
          days) %>%
  filter(days < 1000)



# IMPUTE DAYS BETWEEN DATA ----

full_data <- NULL

for(i in seq_along(unique(outbreak_data$outbreak))){
  
  current_data <- outbreak_data %>% 
    arrange(days) %>%
    filter(outbreak == unique(outbreak_data$outbreak)[i])
  
  max_days <- max(current_data$days)
  
  for(j in 2:nrow(current_data)){
    
    current_data$deaths[j] <- max(current_data$deaths[j],
                                  current_data$deaths[j-1])
    
  }
  
  fn <- splinefun(current_data$days, 
                  current_data$deaths, 
                  method='hyman')
    
  prediction_df <- data.frame(outbreak = unique(outbreak_data$outbreak)[i],
                              days = 0:max_days,
                              deaths = fn(0:max_days),
                              stringsAsFactors = FALSE)

  full_data <- bind_rows(full_data,
                         prediction_df)
  
}



# ANIMATION ----

# * Set Animation Schedule ----

stall_point <- full_data %>% 
  filter(outbreak == 'COVID-19 (2019)') %>% 
  summarize(max(days)) %>% 
  unlist()

iteration_sequence <- c(1:(stall_point-3),
                        rep(stall_point-2, 2),
                        rep(stall_point-1, 2),
                        rep(stall_point, 12),
                        rep(stall_point+1, 2),
                        rep(stall_point+2, 2),
                        seq(stall_point+3, stall_point+30, length = 15),
                        seq(stall_point+31, stall_point+100, length = 15),
                        seq(stall_point+100,400, length = 30),
                        rep(400, 20))

# * Create Animation Data ----

animation_data <- NULL

for(i in seq_along(iteration_sequence)){

  animation_data <- 
    bind_rows(animation_data,
              full_data %>% 
                filter(days == round(iteration_sequence[i])) %>% 
                mutate(iteration = i))
  
}

# * Watermark ----

watermark <- animation_data %>% 
  group_by(iteration) %>% 
  summarize(days = max(days) / 10, 
            deaths = max(deaths) * .9,
            label = '@LukeBornn') %>% 
  ungroup()


for(i in 2:nrow(watermark)){
  watermark$deaths[i] <- max(watermark$deaths[i],
                          watermark$deaths[i-1])
  
}

# * Build Animation ----

my_anim <- ggplot(animation_data, 
                  aes(days, 
                      deaths, 
                      color = outbreak, 
                      group = outbreak)) + 
  geom_line() + 
  geom_segment(aes(xend = days, 
                   yend = deaths), 
               linetype = 2, 
               colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 1.02 *days, 
                label = outbreak, 
                size = 3), 
            hjust = 0) + 
  geom_text(aes(x = 1.4 * days + 10, 
                label = ""), 
            hjust = 0) + 
  geom_text(inherit.aes = FALSE, 
            data = watermark,
            aes(x = days / 50,
                y = deaths, 
                label = label,
                alpha = .1), 
            hjust = 0) + 
  transition_reveal(iteration) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Number of Deaths 
                {round(iteration_sequence[frame_along])} Days After Outbreak', 
       x = "Days", 
       y = 'Deaths') + 
  theme_minimal() + 
  scale_y_continuous(labels = comma)+
  theme(plot.margin = margin(8, 8, 8, 8),
        legend.position = "none") +
  view_follow(aspect_ratio = 1.3)

animate(my_anim, 
        duration = 30, 
        fps = 5, 
        width = 500, 
        height = 300, 
        renderer = gifski_renderer())
anim_save("outbreak_animation.gif")




