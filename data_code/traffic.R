#Makes chart from airmasterk showing mean pm2.5 at each hour of the day



airmasterk <- airmasterk %>%
  mutate(
    dayofweek = wday(Time),
    hour = hour(Time),
  )

airmasterk <- airmasterk %>%
  filter(
    dayofweek > 1,
    dayofweek < 7,
    pm2.5 > 0,
    pm2.5 < 200
    )

airmasterk <- airmasterk %>%
  group_by(hour) %>% #group by the hour
  summarise(hour_mean = mean(pm2.5, na.rm = TRUE)) #hourly pm2.5 mean



ggplot(data =  airmasterk,aes(x = hour,y = hour_mean) ) +
  geom_point(aes(y = hour_mean), color = "red", size= 1) +
  geom_line(aes(y = hour_mean)) +
  scale_y_continuous(limits = c(2,5)) +
  labs(
    y = "PM2.5",
    x = "Hour",
    title = paste(
      "2020 Particulate Matter - Kingston"
    ),
    subtitle = paste(
      "PM2.5 by hour - Weekdays"
    ))
