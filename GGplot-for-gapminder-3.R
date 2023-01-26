library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
head(gapminder)
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
past_year <- 1970
present_year <- 2010
west <- c("Western Europe", "Northern Europe", "Southern Europe", "North America", "Australia and New Zealand")
country_list_1 <- gapminder %>% filter(year == past_year & !is.na(gdp)) %>% .$country
country_list_2 <- gapminder %>% filter(year == present_year & !is.na(gdp)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)
length(country_list)

 
#histogram for dollars per day 
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

#histogram for dollars per day (log2 scale)
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")
#we can see that there are 2 modes: 1st is at $2(2^1) and the 2nd is at $32 (2^5)

#histogram for dollars per day with log transformed
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")
#we can see the x value for log2 is transformed so that we don't need to calculate the 2^1, 2^5 etc.

# compare dollars per day geographically
length(levels(gapminder$region))

gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# order the boxplots in ascending
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% #reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) + # try "fill" vs "color"
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") + #to see the differences for low income regions
  geom_point(show.legend = FALSE) + #to show data points
  xlab("")

# compare dollars per day overtime
gapminder %>% 
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~group)
  
# compare the dollars per day between 1970 and 2010 and between West and developing regions
gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~group)

# compare which region improved the most from 1970 to 2010
gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  facet_grid(year~.)

#improved model of above
gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = factor(year))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")

#density plot
  # look at the group sizes of West and Developing
gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  summarize(count_West = sum(group == "West")/2, count_developing = sum(group == "Developing")/2)
  
  # make the density plot area ratio to the group sizes
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) + # "fill" will group the data by "group" and fill with different color"; 
  # "..count.." adjust the y axis to make the size of density area proportional to the count in each group
  geom_density(alpha = 0.2, bw = 0.75) + #alpha make the overlap color more visible; bw make the curve smoother
  scale_x_continuous(trans="log2") +
  facet_grid(year ~ .)

# take a deeper look into which specific regions improve the most
# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + #make the graphs stack on each other to make the picture clearer
  scale_x_continuous(trans="log2") +
  facet_grid(year ~ .)

#some country has larg popluation such as China, make the density area proportional to the populaiton
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) + #replace the "..count.." with "weight" for density area proportional based on population
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  scale_x_continuous(trans="log2") +
  facet_grid(year ~ .)

# plot infant survival versus income, with transformed axes
gapminder <- gapminder %>%
  mutate(group = case_when(
    region %in% west ~ "The West",
    region %in% "Northern Africa" ~ "Northern Africa",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region == "Southern Asia" ~ "Southern Asia",
    region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
    region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365, infant_suvival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income
surv_income %>% arrange(income)

surv_income %>% ggplot(aes(income, infant_suvival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) + 
  scale_y_continuous(trans = "logit", limit = c(0.875, 0.9981, breaks = c(.85, .95, .99, .995, .998))) +
  geom_label(size = 3, show.legend = FALSE)











