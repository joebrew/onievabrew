library(readxl)
library(dplyr)
library(gsheet)
df <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/188-PMOOi-LftuvJa9ILHYRBLhmeBNh2oxVVPC1Es_zI/edit#gid=0')

locations <- sort(unique(df$location))
keys <- sort(unique(df$key))
model <- function(the_location = 'Andalucia',
                  the_key = 'Casos nuevos'){
  model_data <- df %>%
    filter(location == the_location,
           key == the_key) %>%
    filter(!is.na(value)) %>%
    mutate(value = ifelse(value == 0, value + 0.1, value))
  if(nrow(model_data) > 0){
    fit <- lm(log(value) ~ date, data = model_data,
              weights = date - min(date))
    fake_data <- tibble(date = seq(min(model_data$date),
                                   max(model_data$date) + 5,
                                   by = 1),
                        key = the_key,
                        location = the_location) %>%
      left_join(model_data) %>%
      mutate(predicted = NA)
    preds <- predict(fit, newdata = fake_data)
    fake_data$predicted <- exp(preds)
    return(fake_data)
  } else {
    return(NULL)
  }
}

counter <- 0
out_list <- list()
for(l in locations){
  for(k in keys){
    out <- model(the_location = l,
                 the_key = k)
    if(!is.null(out)){
      counter <- counter +1  
      message(counter)
      out_list[[counter]] <- out
    }
  }
}

done <- bind_rows(out_list)

dir.create('onieva')
library(ggplot2)
for(l in locations){
  for(k in keys){
    sub_done <- done %>% filter(location == l,
                                key == k)
    g <- ggplot(data = sub_done,
           aes(x = date,
               y = value)) +
      geom_line(color = 'red', alpha = 0.4, size = 2) +
      geom_point(color = 'red', alpha = 0.8, size = 5) +
      geom_line(aes(y = predicted),
                lty = 2, alpha = 0.9,
                size = 2) +
      theme_bw() +
      labs(x = 'Fecha',
           y = 'Valor',
           title = paste0(k, ', ', l),
           subtitle = 'Modelo básico log-lineal')
    ggsave(paste0('~/Desktop/onieva/', l, '_', k, '.png'))
  }
}

