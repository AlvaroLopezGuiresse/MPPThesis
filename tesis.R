
# Load packages
library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(tidyr)
library(zoo)
library(countrycode)

setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.1] Corruption as policy failure/tesis/20171028TESIS")
rm(list = ls())

# DATA MANAGEMENT IDEA APPENDIX
#################################
years <- c(2006:2015)

countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "GTM", 
               "HND", "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "URY", "VEN")

clean_text <- function(whyisthis) {
  
  # Detect numbers and sub them
  years <-  stri_extract_all_regex(whyisthis, "\\d{4}") %>% unlist() %>%
    as.numeric()
  
  years <- years[years <= 2016 & years >= 1900]
  if (length(years) == 0) return(NA)
  if (is.na(years)) return(NA)
  
  # Return output
  return(max(years, na.rm = T))
}

# import IDEA appendix
idea_appendix <- import("Libro1.xlsx")[, 1:4]
idea_appendix <- idea_appendix %>% fill(Country)
idea_appendix_0 <- idea_appendix

# extract year of enforcement from Attribution
temp_idea_appendix <- rowwise(idea_appendix)
temp_idea_appendix <- mutate(temp_idea_appendix, 
                             year_enforcement = clean_text(Attribution))

# create several variables... with separate...
temp_idea_appendix <- separate(data = temp_idea_appendix, 
                                  col = `Used in Question`, 
                                  fill = "right", 
                                  into = paste0("temp_", 1:43))

# gather temporal variables to extract the Questions which were enacted...
temp_idea_appendix_01 <- gather(data = temp_idea_appendix, 
                                key = Temp, 
                                value = Question, 
                                ... = starts_with("temp_"))

# temp_idea_appendix_01 <- arrange(temp_idea_appendix_01, Country, Attribution, Dummies)
temp_idea_appendix_01 <- filter(temp_idea_appendix_01, !is.na(Question))
temp_idea_appendix_01 <- select(temp_idea_appendix_01, -Temp)

# After every year-question are togheter for every country... 
# variables for every Question are created... and a value of 1 will be assigned,
# to identify the years along with the question variables (when later spread as columns)
temp_idea_appendix_02 <- mutate(temp_idea_appendix_01, 
                                Question = paste0("Ques_", Question), 
                                temp = 1)

# group by ... to delete duplicates... n() the frequency of every grouped single observation...
# every question should be unique... by year by legal source and by country...
# later dupicates across time (i.e. question legislated in several years - Q2 was legislated in 2009 and 2012)
# will be taken the oldest one...
temp_idea_appendix_02 <- group_by(temp_idea_appendix_02, Country, Attribution, Question)
temp_idea_appendix_02 <- mutate(temp_idea_appendix_02, drop = seq(1:n()))
temp_idea_appendix_02 <- filter(temp_idea_appendix_02, drop != 2)
temp_idea_appendix_02 <- select(temp_idea_appendix_02, -drop)






# -> all of the legal sources are linked to a question for every year in a long dataset.
# -> temp_idea_appendix_03






# variables for every Question are spread into columns... the value of 1 represents a question (law)
# being enacted in a specific year...
temp_idea_appendix_03 <- spread(data = temp_idea_appendix_02, 
                                key = Question, 
                                value = temp, 
                                fill = 0)

# variables are re-ordered...
temp_idea_appendix_03 <- select(temp_idea_appendix_03, Country, Attribution, Type, 
                                year_enforcement, paste0("Ques_", 1:43))

# final dataset
idea_appendix <- temp_idea_appendix_03

# iso3c is added...
idea_appendix$iso3c <- countrycode(idea_appendix$Country, 'country.name', 'iso3c', warn = FALSE)
idea_appendix <- idea_appendix %>% select(Country, iso3c, everything())

# LATAM sample is selected... questions which couldn't be coded are 
# droped, missing values ie. years that couldn't be traced are also dropped...
drop <-  paste0("Ques_", c(14, 16, 18, 20, 21, 22, 24, 27, 32, 34, 40, 42))

idea_appendix <- idea_appendix %>% 
  filter(iso3c %in% countries) %>% 
  filter(!is.na(year_enforcement))

idea_appendix <- idea_appendix[ , -which(names(idea_appendix) %in% drop)] %>% 
  as.data.frame()

rm(list = ls()[!ls() %in% c("idea_appendix_0", "idea_appendix", "countries", "years")])
# DATA MANAGEMENT IDEA APPENDIX
#################################

# DATA MANAGEMENT IDEA SURVEY 
##############################
df <- read.csv("questionario_csv.csv")
idea_survey <- df
idea_survey_0 <- idea_survey

idea_survey <- idea_survey %>% select(-c(X14, X16, X18, X20, X21, X22, X24, X27, X32, X34, X40, X42))
idea_survey <- idea_survey %>% filter(ISO %in% countries) %>% rename(iso3c = ISO)
idea_survey$iso3c <- idea_survey$iso3c %>% as.character()


# QUESTION...
# idea_survey <- df
# temp <- idea_survey %>% mutate_at(.vars = vars(X1:X43), 
#                                   .funs = funs(ifelse(. == "No data", NA,.)))


# idea_survey_temp <- idea_survey %>% as.character()
# 
# idea_survey_temp <- idea_survey %>% 
#   mutate_at(.vars = vars(X1:X43), 
#             .funs = funs(ifelse(. == "No data", NA,.)))





# changes "No data" by NA...
for (j in 3:ncol(idea_survey)){
  for (i in 1:nrow(idea_survey)){
    
    if (idea_survey[i,j] == "No data"){
      idea_survey[i,j] <- NA
      
    } else{
      idea_survey[i,j] <- idea_survey[i,j]
    }
  }
  
  # QUESTION...
  idea_survey[,j] <- as.character(idea_survey[,j])
  idea_survey[,j] <- as.numeric(idea_survey[,j])
}


# appendix and survey datasets are combined...  
idea_combined <- full_join(idea_survey, idea_appendix, by = "iso3c")

idea_combined <- idea_combined %>% 
  select(-Country.y) %>% 
  select(Country.x, iso3c, year_enforcement, Attribution, Type, everything()) %>% 
  rename(country = Country.x,
         attribution = Attribution,
         type = Type)

temp <- idea_combined





# QUESTION...
# idea_survey <- df
# temp <- idea_survey %>% mutate_at(.vars = vars(X1:X43), 
#                                   .funs = funs(ifelse(. == "No data", NA,.)))





# Columns multiplication: The variables in the appendiz are multiplied by
# the variables in the survey...
for(j in 6:36){
  for(i in 1:nrow(temp)){
    
    # temp has 67 variables
    if (is.na(temp[i, j])){
      temp[i, 67 + 1 + (j - 6)] <-  NA
      
    } else{
      temp[i, 67 + 1 + (j - 6)] <- temp[i, j] * temp[i, 36 + 1 + (j - 6)]
      names(temp)[67 + 1 + (j - 6)] <-  paste0("Q", names(temp)[36 + 1 + (j - 6)] %>% 
        str_extract("\\-*\\d+\\.*\\d*"))
  
    }
  }
}

# select variables for the panel... the others are dropped...
temp_01 <- temp %>% 
  select(country, iso3c, year_enforcement, Q1:Q43)






# IMPORTANT...
# combine rows by year...
# i.e: ARG has for Q6 a value of 1 in year 2009 & 2016... -> ??...still pending...? (1)
# i.e: BRA has for Q1 a value of 2 in year 2010...        -> this is addressed. (2)

temp_02 <- temp_01 %>% 
  group_by(year_enforcement, iso3c) %>% 
  summarise_at(.vars = names(.)[4:34],
               .funs = c(sum = "sum")) %>% 
  arrange(iso3c, year_enforcement)

# repeated values are replaced... (2)
temp_03 <- temp_02 %>% 
  mutate_at(.vars = vars(Q1_sum:Q43_sum), 
            .funs = funs(ifelse(. > 1, 1, .)))








# DATA HELP DESK CJ 15-11-2017
# spread questions to build the panel...
temp_04 <- temp_03 %>% 
  ungroup() %>% 
  tidyr::gather("question", "value", -year_enforcement, -iso3c) %>% 
  dplyr::filter(value == 1) %>% 
  # filter(iso3c == "CHL") %>% 
  group_by(iso3c, question) %>% 
  filter(year_enforcement == min(year_enforcement)) %>% 
  tidyr::spread(question, value) %>% 
  arrange(iso3c, year_enforcement)









# NOT SO IMPORTANT...
# arrange the names...
n <- names(temp_04)
n_temp <- n 
for (i in 1:length(n)){
  n_temp[i] <- n[i] %>% 
    stringr::str_extract("\\-*\\d+\\.*\\d*")

}

t <- n_temp
t <- t[2:length(t)]
t <- t %>% as.numeric 
t <- sort(t)
t <- paste0("Q", t)
t <- paste0(t, "_sum")

# arrabge the names...
temp_04 <- select(temp_04, iso3c, year_enforcement, t)

# define years in the panel by vector years_panel...
# posible years... panel countries + years...
years_panel <- unique(temp_04$year_enforcement) %>% sort()
years_panel <- c(years_panel[1:13], 2006:2015) 

# build matrix with years and countries...
t0 <- cbind(countries[1], years_panel)
for(i in 2:length(countries)){
  t1 <- cbind(countries[i], years_panel)
  t <- rbind(t0, t1) 
  t0 <- t
  
}

years_iso3c <- t %>% as.data.frame()
temp_idea_panel <- temp_04





rm(list = ls()[!ls() %in% c("idea_appendix_0", "idea_appendix", "idea_survey_0","idea_survey",
                            "temp_idea_panel", "years_iso3c")])
# DATA MANAGEMENT IDEA SURVEY 
##############################






# QUESTION...
# GENERATES A PROBLEM IN THE PANEL if a question is linked twcie across time?
# if so... can I keep the older one?

# based on temp_idea_panel a panel dataset should be built on long format...

temp_idea_panel_01 <- temp_idea_panel %>% 
  mutate(temp_year_enforcement = year_enforcement, temp_na = NA)





# V1
temp_idea_panel_01 <- spread(data = temp_idea_panel_01,
                             key = temp_year_enforcement,
                             value = temp_na) 

temp_idea_panel_01 <- gather(data = temp_idea_panel_01,
                             key = year_panel,
                             value = temp_na,
                             ... = `1925`:`2016`) 

temp_idea_panel_01 <- temp_idea_panel_01 %>% 
  select(iso3c, year_panel, year_enforcement, everything()) %>% 
  select(-temp_na)

temp_idea_panel_02 <- temp_idea_panel_01 %>% 
  gather(key, value, Q1_sum:Q43_sum) %>% 
  arrange(iso3c, key, year_enforcement, year_panel) %>% 
  filter(!is.na(value))
  
# INDICATOR  
# generate indicator by question...
temp_idea_panel_03 <- temp_idea_panel_02 %>% 
  group_by(iso3c, key) %>% 
  arrange(iso3c, key, year_panel) %>% 
  mutate(value = ifelse(year_panel >= year_enforcement, 1,0))



# spread indicator...
temp_idea_panel_04 <- temp_idea_panel_03 %>% 
  spread(year_enforcement, key)


#



















# BORRADOR...
###############################


# join datasets temp_04 and t (years + iso3c)
years_iso3c <- years_iso3c %>% rename(year_enforcement = years_panel)
years_iso3c <- years_iso3c %>% rename(iso3c = V1)
years_iso3c$year_enforcement <- years_iso3c$year_enforcement %>% as.character %>% as.integer()

years_iso3c$iso3c <- years_iso3c$iso3c %>% as.character()

temp <- full_join(years_iso3c, temp_idea_panel, by = c("iso3c", "year_enforcement"))

# do not rename in order to join datasets ... to keep both variables...
# then gather, and then spread...






# 
temp <- temp %>% group_by(iso3c) %>% 
  mutate_at(vars(Q1_sum:Q43_sum), funs(zoo::na.locf(., na.rm = F))) %>% 
  mutate_at(vars(Q1_sum:Q43_sum), funs(ifelse(is.na(.), 0, .))) 



# QUESTION...
# group by COUNTRY then make an ifelse... and mutate all of the observations in that group... 
# with a condition from the summarize thing...
temp_01 <- temp %>% group_by(iso3c) %>% 
  mutate_at(vars(Q1_sum:Q43_sum), funs())

#



# borrador...
df1 <- data_frame(
  pais = c("arg", "arg", "arg", "arg", "arg", 
           "chl", "chl", "chl", "chl", "chl"),
  year = c(1990:1994,
           1990:1994),
  value1 = c(NA, NA, 1, NA, NA,
           NA, NA, 1, NA, NA),
  value2 = c(NA, NA, NA, NA, NA,
             NA, NA, NA, 1, NA),
  value3 = c(NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA))

df2 <- df1 %>% group_by(pais) %>% 
  mutate_at(vars(contains("value")), funs(zoo::na.locf(., na.rm = F)))

df2 <- df2 %>% group_by(pais) %>% 
  mutate_at(vars(contains("value")), funs(ifelse(is.na(.), 0,.)))

df3 <- df2 %>% group_by(pais) %>% summarise_at(vars(contains("value"), funs(sum()))
  

#


# create variables for making logical test...  
for(i in 6:9){
  df2[,i] <- NA
  
}

df2 <- df1 %>% group_by(pais) %>% 
  mutate_at(vars(V6:V8), funs(sum(is.na(.))))


dataframe <- data_frame(helloo = c(1,2,3,4,5,6),
                        ooooHH = c(1,1,1,2,2,2),
                        ahaaa = c(200,400,120,300,100,100))

dataframe %>% mutate_at(vars(contains('oo')), .funs = funs(cat = ntile(., 2)))

by_species %>% mutate_all(funs(. / 2.54))

df <- data_frame(x = c(TRUE, TRUE, FALSE),
                 y = c("Hello", "Hola", "Ciao"),
                 z = c("World", "ao", "HaOlam")
)

df %>%
  mutate_at(.vars = vars(y, z),
            .funs = funs(ifelse(x, ., NA)))

df %>%
  mutate_at(.vars = vars(y, z),
            .funs = funs(ifelse(x, ., NA)))

# df2 <- df1 %>% group_by(pais) %>% 
#   mutate(value4 =, funs(zoo::na.locf(., na.rm = F)))
#

idea_re_combined <- temp_03

# posible years... panel countries + years...
years <- unique(temp_01$year_enforcement) %>% sort()
years <- c(years[1:13], 2006:2015) 

t0 <- cbind(countries[1], years)
  for(i in 2:length(countries)){
    
    t1 <- cbind(countries[i], years)
    t <- rbind(t0, t1) 
    t0 <- t
    
  }

t <- t %>% as.data.frame() %>% rename(iso3c = V1)
idea_re_combined <- idea_re_combined %>% mutate(years = year_enforcement)

temp_panel <- full_join(idea_re_combined, t, by = "iso3c")

temp_panel <- temp_panel %>% select(iso3c, years.y, year_enforcement,everything())

rm(list = ls()[!ls() %in% c("idea_appendix", "countries", "years")])

# peueba


df1 <- data_frame(
  year = c(1990:1994),
  pais = c("arg", "arg", "chl", "chl", "chl"),
  id = c("id1", "id1",  "id1",  "id2", "id3"),
  freq = c(5,10, 10,15,5)
)

df2 <- data_frame(
  year = c(1990:1994),
  pais = c("arg", NA, "chl", "chl", "chl"),
  id = c("id1", NA,  "id1",  "id2", "id3"),
  freq = c(5,NA, 10,15,5)
)

df2 <- df1 %>% mutate_at(.vars = vars(id:freq), .funs = funs(ifelse(is.na(pais), NA,.)))

df1 <- data_frame(x = c(TRUE, TRUE, NA),
                 y = c("Hello", "Hola", "Ciao"),
                 z = c("World", "ao", "HaOlam")
)

df2 <- data_frame(x = c(TRUE, TRUE, FALSE),
                  y_1 = c("Hello", "Hola", NA),
                  z_1 = c("World", "ao", NA) 
)

df %>%
  mutate_at(.vars = vars(y, z),
            .funs = funs(ifelse(is.na(x), NA,.)))

df1 <- data_frame(
  a1 = c(1:5),
  a2 = c(6:10),
  a3 = c(11:15),
  a4 = c(16:20),
  b1 = c(1:5),
  b2 = c(6:10),
  b3 = c(11:15),
  b4 = c(16:20)
)

names(bankdata)[(5:7)+3] =  paste0(names(bankdata)[5:7], 'toAssest')
