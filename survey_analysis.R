# R Survey analysis

#### Read in packages and files ####

# Standard text mining package
#library(limonade)
library(dplyr)
library(tm) # https://www.r-bloggers.com/2021/05/sentiment-analysis-in-r-3/
library(deeplr) # For translating French surveys
library(readr)
library(ggplot2)

library(ggmap) # For university map
library(maps)

# Need to create our own detect function due to a bug in deeplr:
# https://github.com/zumbov2/deeplr/issues/6
Translate <- function(text, auth_key = DEEPL_API_KEY){
  
  text_positions <- which(!is.na(text))
  
  language <- translate2(text[text_positions], 
                      source_lang = "FR",
                      split_sentences = FALSE,
                      auth_key = auth_key)
  
  new_vec <- rep(NA_character_, length(text))
  new_vec[text_positions] <- language
  
  return(new_vec)
}


DEEPL_API_KEY = readLines("credentials/deepl_api_key.txt")
GMAPS_API_KEY = readLines("credentials/gmaps_api_key.txt")

#register_google(GMAPS_API_KEY)

raw_survey  = read_csv("raw_data/results-survey961781.csv", na = c("", "NA", "N/A"))
#raw_survey_fr  = read_csv("raw_data/results-survey961781_fr.csv")

#### Text mining ####

## Q0 - Chart

# https://www.techwalla.com/articles/how-to-scroll-an-image-in-powerpoint

##Q1 - Are you going to be honest?

# Summary
table(raw_survey$`Do_you_commit_to_providing_thoughtful_answers_to_these_questions?`)
# Everyone said yes with one cheeky exception

##Q2 - Are you studying at uni now?

# Combine this with people who have every studied
table(raw_survey$`Are_you_currently_studying_at_a_university_level_or_higher?`)
# About half half

##Q3 - How old are you?

hist(raw_survey$`How_old_are_you?`, breaks = 10)
# Representative of people around our age

##Q4  - Have you ever studied at university?

raw_survey[,c("Are_you_currently_studying_at_a_university_level_or_higher?", "Have_you_ever_studied_at_university?")]

uni_status <- rep(NA_character_, nrow(raw_survey))
uni_status[raw_survey$`Are_you_currently_studying_at_a_university_level_or_higher?` == "Yes"] <- "Currently studying" 
uni_status[raw_survey$`Have_you_ever_studied_at_university?` == "Yes"] <- "Previously studied"
uni_status[raw_survey$`Have_you_ever_studied_at_university?` == "No"] <- "Never studied"

current_study_data <- data.frame(status = uni_status, age = raw_survey$`How_old_are_you?`)

# level of study
ggplot(current_study_data, aes(x = uni_status)) +
  geom_bar()

# age vs level of study
ggplot(current_study_data, aes(x = age, y = uni_status)) +
  geom_point()

anova(aov(age~uni_status, data = current_study_data))
# No significant difference between categories

##Q5 - Name of university

# uni_locations <- geocode(na.omit(raw_survey$`What's_the_name_of_the_university_where_you_last_studied?`))
# 
# map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
# points(uni_locations$lon, uni_locations$lat, col="red", pch=16)

##Q6 -  Level of attainment

##Q7 - Name of degree

# Translation error

##Q8 - Have you received a scholarship?

##Q9 - Who provided you with this scholarship?

##Q10 - How helpful was the scholarship?

##Q11 - Who benefits from scholarships?

# Nobody put other :(

##Q12 - How would you evaluate the benefit to these parties?

# Problem, we've already established that they benefit, 

## Q15 - Any other thoughts
# images/need_french.png
# images/deepl.png
# Had to file a bug report
# images/utf8_error.png
# images/bug_reoprt.png
# Translate all French into English
raw_survey <- raw_survey %>% 
  mutate(`Do_you_have_any_general_thoughts_about_scholarships_that_you_would_like_to_share?` = 
           if_else(Start_language == "fr", 
                   Translate(`Do_you_have_any_general_thoughts_about_scholarships_that_you_would_like_to_share?`),
                   `Do_you_have_any_general_thoughts_about_scholarships_that_you_would_like_to_share?`
           )
  )


#### Clustering #####

# Maybe on scholarship opinions age and educational attainment?

#### Score analysis ####


