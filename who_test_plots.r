### shinydashboard WHO CDCC Deaths 2020: plots tests
# author: remy
# date: 13/01/2021

# load the filtered data without "X1" column and recode
cdcc_deathprob <- read_csv("~/Documents/who_2020_shinydashboard/who_2020_data/cdcc_filtered.csv") %>%
  select(-X1) %>%
  # recode location and sex as factors
  mutate(location = factor(location),
         sex = factor(sex))

## filtered data for "summary" tab; used as reactives in app
# mean percent overall
cdcc_deathprob %>%
  # filter for the values of interest (later inputs)
  filter(year == 2016,
         sex == "Both sexes") %>%
  # calculate the mean probability for that year and both sexes then pull that value from the dataframe
  summarise(mean_percent = mean(percent)) %>% pull()

# country with lowest probability 
cdcc_deathprob %>%
  # see above
  filter(year == 2016,
         sex == "Female") %>%
  # order the dataframe by descending probability, then pull the location from the dataframe as a character string
  arrange(desc(percent)) %>% head(1) %>% pull(location) %>% as.character()

# biggest changes from 2000 to 2016
cdcc_deathprob %>%
  # see above
  filter(year %in% c(2000, 2016)) %>%
  # transform the "year" column from long to wide for easier column-wise calculations
  pivot_wider(names_from = year, values_from = percent) %>%
  # calculate the difference in probability between year 2016 and 2000
  mutate(diff = `2016` - `2000`) %>%
  # arrange this new column in descending order
  arrange(desc(diff))

# mean decrease in probability
cdcc_deathprob %>%
  # see above
  filter(year %in% c(2000, 2016)) %>%
  # see above
  pivot_wider(names_from = year, values_from = percent) %>%
  # see above
  mutate(diff = `2016` - `2000`) %>%
  # calculate mean difference
  summarise(mean_diff = mean(diff, na.rm = TRUE))

# plot the timeseries per country
cdcc_deathprob %>%
  # filter for particular country (will become loaction input)
  filter(location == "Switzerland") %>%
  # plot probability agains year, color by sex
  ggplot(aes(x = year, y = percent, color = sex)) +
  # line plot
  geom_line(lwd = 1.5) +
  # scaterplot
  geom_point(size = 3) +
  # cosmetics to make plot pretty
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2016)) +
  scale_color_manual(values = wes_palette("Moonrise2")[c(1,2,4)]) +
  labs(title = paste0("Probability of dying from CDCC diseases between Age 30 - 70 in ", "location"),
       x = "Year",
       y = "Probability to die from CDCC diseases (%)") +
  theme_light()

# calc the difference between the current year and the previous one per country, by sex and plot
cdcc_deathprob %>%
  # order year in ascending fashion (otherwise mutate downstream does not work)
  arrange(year) %>%
  # see above
  filter(location == "Germany") %>%
  # groupy the dataframe first by lcation, then year
  group_by(location, sex) %>%
  # add new column, calculte the yearly difference in porbability (means 2005 vs. 2000, 2010 vs. 2005 etc.)
  mutate(diff_year = percent - dplyr::lag(percent)) %>%
  # plot the new difference in probability of a current year compared to the previous vs the year, colored by sex
  ggplot(aes(x = year, y = diff_year, color = sex)) +
  # line plot
  geom_line(lwd = 1) +
  # scatterplot
  geom_point(size = 3) +
  # horizontal y = 0 line
  geom_hline(yintercept = 0, lwd = 0.5, linetype = "longdash", color = "black") +
  # cosmetics
  scale_x_continuous(limits = c(2005, 2016) ,breaks = c(2005, 2010, 2015, 2016)) +
  scale_y_continuous(limits = c(-3, 3)) +
  scale_color_manual(values = wes_palette("Moonrise2")[c(1,2,4)]) +
  labs(title = paste0("Difference in probability between the current and the previous year in ", "location"),
       x = "Year",
       y = "Difference in probability to the previous year (%)") +
  theme_light()