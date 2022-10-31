# Created with R version 4.1.2 (2021-11-01) 

library(magrittr) # assignment pipe
library(tidyverse) # default awesome functions
library(readxl) # alternative excel reader

library(glue) # alternative to paste
# set the earliest date only include data rom this year
earliest_date <- dmy(05012022)

# get c19 data ----
## DHB------
mort_national <-  read_csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/weekly-deaths.csv") %>%
  janitor::clean_names()
names(mort_national)[2:3] <- c("death_within28days", "death_attr")

moh_covid_case_raw <-  read_csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/covid-cases.csv") %>%
  janitor::clean_names() %>%
  mutate(week_end_date = ceiling_date(report_date , "week")) %>%
  rename(date = report_date)
  
# couldn't figure out to directly access this file, had to download.
moh_covid_hosp <-  read_excel(here::here("data/data_uploaded/covid-cases-in-hospital-counts-location.xlsx"),
                                  sheet = "NZ total") %>%
  janitor::clean_names() %>%
  mutate(week_end_date = ceiling_date(date, "week"),
         week_end_date = ymd(week_end_date),
         hosp = as.numeric(hosp)) %>%
  group_by(week_end_date) %>%
  summarise(hosp = sum(hosp))

### wrangle data-----
moh_case_DHB_daily <- moh_covid_case_raw %>%
  filter(case_status == "Confirmed") %>%
  select(date, dhb) %>%
  group_by(date, dhb) %>%
  summarise(nCases = n())

moh_case_national_daily <- moh_covid_case_raw %>%
  filter(case_status == "Confirmed") %>%
  select(date) %>%
  group_by(date) %>%
  summarise(nCases = n())
####join all data by week date
moh_case_national_weekly <- moh_case_national_daily %>% 
  mutate(week_end_date = ceiling_date(date, "week")) %>% 
  group_by(week_end_date) %>% 
  summarise(nCases = sum(nCases)) %>% 
  inner_join(., mort_national %>% 
               rename(week_end_date = week_ending) %>% 
               filter(week_end_date >= earliest_date)) %>% 
  inner_join(moh_covid_hosp) %>% 
  filter(week_end_date >= earliest_date)

# EDA----
  GGally::ggpairs(moh_case_national_weekly)+
  theme_classic()  
# plot each count measure over time on its own natural scale  
  moh_case_national_weekly %>%
  pivot_longer(cols = -week_end_date, 
               names_to = "measure", 
               values_to = "count") %>% 
  ggplot(aes(x = week_end_date, y = count, colour = measure))+
  geom_line()+
  scale_x_date(labels = scales::label_date_short(),
               date_breaks = "2 week",
               name = "")+
  scale_y_log10()+
  theme_classic()
  
# scatterplot of Cases vs death_within28days----
  library(ggpmisc)
moh_case_national_weekly %>% 
  ggplot(aes(y = nCases, x = death_within28days))+
  geom_point()+
  geom_smooth(method = glm, se = TRUE, method.args = list(family = "poisson"))+
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  theme_classic()  
par(mfrow = c(2,2))
plot(glm(nCases ~ death_within28days , data = moh_case_national_weekly, family = "poisson"))

# scatterplot of Cases vs death_attr----
moh_case_national_weekly %>% 
  ggplot(aes(y = nCases, x = death_attr))+
  geom_point()+
  geom_smooth(method = glm, se = TRUE, method.args = list(family = "poisson"))+
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  theme_classic()  
par(mfrow = c(2,2))
plot(glm(nCases ~death_attr , data = moh_case_national_weekly, family = "poisson"))

# scatterplot of Cases vs hosp----
moh_case_national_weekly %>% 
  ggplot(aes(y = nCases, x = hosp))+
  geom_point()+
  geom_smooth(method = glm, se = TRUE, method.args = list(family = "poisson"))+
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  theme_classic()  
par(mfrow = c(2,2))
plot(glm(nCases ~ hosp, data = moh_case_national_weekly, family = "poisson"))

# scatterplot of hosp vs death_attr----
moh_case_national_weekly %>% 
  ggplot(aes(x = hosp, y = death_attr))+
  geom_point()+
  geom_smooth(method = glm, se = TRUE, method.args = list(family = "poisson"))+
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  theme_classic()  
par(mfrow = c(2,2))
plot(glm(death_attr ~ hosp, data = moh_case_national_weekly,family = "poisson"))
