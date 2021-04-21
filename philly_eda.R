## load libraries and data
# libraries to include
library(tidyverse)
library(lubridate)

# libraries for veil of darkness test
library(lutz)
library(suncalc)
library(splines)

# load stops data
data_path <- "Data/pa_philadelphia_2020_04_01.csv"
stops <- read_csv(data_path)

# additional data
center_lat <- 39.9525839
center_lng <- -75.1652215
population_2017 <- tibble(
  subject_race = c(
    'asian/pacific islander', 'black', 'hispanic', 'other/unknown', 'white'
  ),
  num_people = c(110864, 648846, 221777, 39858, 548312)
) %>%
  mutate(subject_race = as.factor(subject_race))

## initial data cleaning
# other + unknown = other/unknown
stops$subject_race <- recode(stops$subject_race,
  "other" = "other/unknown",
  "unknown" = "other/unknown"
)

# filter pre-2018
stops <- stops %>% filter(year(date) < 2018)

# filter vehicular
stops <- stops %>% filter(type == 'vehicular')

## initial eda
# num. stops per year
stops %>%
  mutate(year = year(date)) %>%
  count(year)

# num. stops per race
stops %>%
  count(subject_race) %>%
  mutate(prop = n/sum(n))

# num. stops per year by race
stops %>%
  count(year = year(date), subject_race) %>%
  ggplot(aes(x=year, y=n, color=subject_race)) +
  geom_point() +
  geom_line()

## further data cleaning
# filter 2017
stops <- stops %>% filter(year(date) == 2017)

## benchmark test
# pop. by race
population_2017 %>%
  mutate(prop = num_people / sum(num_people))

# stop rate by race
stops %>%
  count(subject_race) %>%
  left_join(
    population_2017,
    by = 'subject_race'
  ) %>%
  mutate(stop_rate = n / num_people)

# frisk rate by race
stops %>%
  group_by(subject_race) %>%
  summarize(
    search_rate = mean(search_conducted, na.rm=T),
    frisk_rate = mean(frisk_performed, na.rm=T)
  )

## outcome test
# hit-rate by race
stops %>%
  filter(search_conducted) %>%
  group_by(subject_race) %>%
  summarize(
    hit_rate = mean(contraband_found, na.rm=T)
  )

# hit-rate by race and district
hit_rates <-
  stops %>%
  filter(search_conducted & subject_race %in% c('black', 'white', 'hispanic')) %>% 
  group_by(subject_race, district) %>%
  summarize(hit_rate = mean(contraband_found, na.rm=T)) %>%
  spread(subject_race, hit_rate, fill = 0) %>% 
  rename(white_hit_rate = white) %>% 
  gather(minority_race, minority_hit_rate, c(black, hispanic)) %>%
  arrange(district)

max_hit_rate <-
  hit_rates %>%
  select(ends_with('hit_rate')) %>%
  max() #<--make axes' limits even

search_counts <-
  stops %>%
  filter(
    search_conducted,
    subject_race %in% c('black', 'white', 'hispanic')
  ) %>%
  count(district, subject_race) %>%
  spread(subject_race, n, fill=0) %>%
  rename(num_white_searches = white) %>%
  gather(minority_race, num_minority_searches, c(black, hispanic)) %>%
  mutate(num_searches = num_minority_searches + num_white_searches) %>%
  select(district, minority_race, num_searches) #<--pt. size by num. of searches

hit_rates %>%
  left_join(
    search_counts,
    by = c('district', 'minority_race')
  ) %>%
  ggplot(aes(
    x = white_hit_rate,
    y = minority_hit_rate
  )) +
  geom_point(aes(size=num_searches), pch=21) +
  geom_abline(slope=1, intercept=0, linetype='dashed') + #<--set diag. ref. line
  scale_x_continuous("White hit rate",
    limits = c(0, max_hit_rate+0.01),
    labels = scales::percent
  ) +
  scale_y_continuous("Minority hit rate",
    limits = c(0, max_hit_rate+0.01),
    labels = scales::percent
  ) +
  coord_fixed() +
  facet_grid(cols = vars(minority_race))

# investigate anomalies
hit_rates %>%
  filter(white_hit_rate == min(white_hit_rate))

stops %>%
  filter(district == '77') %>%
  count(location, sort=T)

stops %>%
  filter(search_conducted, district != '77') %>%
  group_by(subject_race) %>%
  summarize(
    hit_rate = mean(contraband_found, na.rm=T)
  )

## veil of darkness test
# get philly timezone
tz <- lutz::tz_lookup_coords(center_lat, center_lng, warn=F)

# helper funct.
time_to_min <- function(time) {
  hour(hms(time)) * 60 + minute(hms(time))
}

# compute sunset times
sunset_times <-
  stops %>%
  mutate(
    lat = center_lat,
    lon = center_lng
  ) %>%
  select(date, lat, lon) %>%
  distinct() %>%
  getSunlightTimes(
    data = .,
    keep = c('sunset', 'dusk'),
    tz = tz
  ) %>%
  mutate_at(vars('sunset', 'dusk'), ~format(., '%H:%M:%S')) %>%
  mutate(
    sunset_minute = time_to_min(sunset),
    dusk_minute = time_to_min(dusk),
    date = ymd(str_sub(date, 1, 10))
  ) %>%
  select(date, sunset, dusk, ends_with('minute'))

# inter_twilight period
sunset_times %>%
  filter(dusk == min(dusk) | dusk == max(dusk))

vod_stops <-
  stops %>%
  left_join(
    sunset_times,
    by = 'date'
  ) %>%
  mutate(
    minute = time_to_min(time),
    minutes_after_dark = minute - dusk_minute,
    is_dark = minute > dusk_minute,
    min_dusk_min = min(dusk_minute),
    max_dusk_min = max(dusk_minute),
    is_black = subject_race == 'black'
  ) %>%
  filter(
    minute >= min_dusk_min,
    minute >= max_dusk_min,
    !(minute > sunset_minute & minute < dusk_minute),
    subject_race %in% c('black', 'white')
  )

# filter 6:30—6:45 window
vod_stops %>%
  filter(time > hm('18:30'), time < hm('18:45')) %>%
  group_by(is_dark) %>%
  summarize(prop_black = mean(is_black))

# model1
mod1 <- glm(
  is_black ~ is_dark + splines::ns(minute, df = 6),
  family = binomial,
  data = vod_stops
)

summary(mod1)$coefficients['is_darkTRUE', c('Estimate', 'Std. Error')]

# model2
mod2 <- glm(
  is_black ~ is_dark + splines::ns(minute, df = 6) + as.factor(district),
  family = binomial,
  data = vod_stops
)

summary(mod2)$coefficients['is_darkTRUE', c('Estimate', 'Std. Error')]
