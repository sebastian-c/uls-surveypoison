# R Survey analysis

#### Read in packages and files ####

# Standard text mining package
#library(limonade) #rsurvey package
# Hadleyversing this as you can see
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(stringr)

library(ggmap) # For university map
library(maps)

library(deeplr) # For translating French surveys
library(tm) # https://www.r-bloggers.com/2021/05/sentiment-analysis-in-r-3/
library(syuzhet) # For sentiment analysis
library(wordcloud)

#### Functions ####

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

selectBrackNames <- function(df, start_char){
  
  base_regex <- ".*?\\[(?!Other)[A-Za-z]+\\]$"
  
  grepl(paste0("^", start_char, base_regex), names(raw_survey), perl = TRUE)
}

extractBrackNames <- function(old_names){
  str_extract(old_names, "(?<=\\[)[A-Za-z]+(?=\\]$)")
}

#### Read in data ####

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

hist(raw_survey$`How_old_are_you?`, 
     breaks = 10,
     main = "How old are you?",
     xlab = "Age")
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
  geom_point() +
  labs(y = "Level of attainment", x = "Age")

# images/age_attainment_anova.png
anova(aov(age~uni_status, data = current_study_data))


# No significant difference between categories

##Q5 - Name of university

# uni_locations <- geocode(na.omit(raw_survey$`What's_the_name_of_the_university_where_you_last_studied?`))
# 
# map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
# points(uni_locations$lon, uni_locations$lat, col="red", pch=16)

##Q6 -  Level of attainment

table(raw_survey$`What_level_of_attainment_did_you_reach_in_your_most_recent_university_experience?`)

##Q7 - Name of degree

# Translation error

##Q8 - Have you received a scholarship?

table(raw_survey$`Have_you_received_a_scholarship_during_your_most_recent_university_studies?`,
      raw_survey$`What_level_of_attainment_did_you_reach_in_your_most_recent_university_experience?`)

# more people got scholarships for masters degrees

##Q9 - Who provided you with this scholarship?

table(raw_survey$`Who_provided_you_with_this_scholarship?`)

##Q10 - How helpful was the scholarship?

table(factor(raw_survey$`How_helpful_was_this_scholarship_to_you_and_the_completion_of_your_studies_on_a_scale_of_1_to_5? __1_=_Not_helpful_at_all5_=_Very_helpful_`,
             levels = 1:5))


##Q11 - Who benefits from scholarships?
sub_scholarship <- raw_survey[, selectBrackNames(raw_survey, "Of")]
sub_scholarship <- rename_with(sub_scholarship, extractBrackNames)

scholarship_plot <- sub_scholarship %>% 
  summarise(across(.fns = \(x) sum(grepl("^Yes$", x)))) %>%
  pivot_longer(everything(), names_to = "Beneficiary", values_to = "n_yes")

ggplot(scholarship_plot, aes(x = Beneficiary, y = n_yes)) +
  geom_col() +
  labs(y = "Number of 'Yes' responses")

# Nobody put other :(

##Q12 - How would you evaluate the benefit to these parties?

# Heatmap

sub_eval <- raw_survey[, selectBrackNames(raw_survey, "How")]
sub_eval <- rename_with(sub_eval, extractBrackNames)

eval_plot <- sub_eval %>% 
  pivot_longer(everything(), names_to = "Beneficiary", values_to = "score") %>%
  group_by(Beneficiary, score) %>%
  summarise(count = n(), .groups = "drop_last")

ggplot(eval_plot, aes(x = Beneficiary, y = score, fill = count)) +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_tile() +
  theme_bw()

#Mainly help students

# Problem, we've already established that they benefit, so we tend to get high values

## Q13 - Any other thoughts
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


corpus_source <- na.omit(raw_survey$`Do_you_have_any_general_thoughts_about_scholarships_that_you_would_like_to_share?`)
corpus <- Corpus(VectorSource(corpus_source))

clean_corpus <- tm_map(corpus, removeWords, stopwords('english'))
clean_corpus <- tm_map(clean_corpus, stemDocument)
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

tdm <- TermDocumentMatrix(clean_corpus)

tdm %>%
  as.matrix %>%
  rowSums %>%
  sort(decreasing = TRUE) %>%
  wordcloud(words = names(.),
            freq = .,
            min.freq = 0,
            colors = brewer.pal(8, 'Dark2')
            )

corpus_source %>%
  get_nrc_sentiment %>%
  colSums %>%
  barplot(xlab = "Sentiment",
          ylab = "Number of responses with sentiment")


#### Clustering #####

# Maybe on scholarship opinions age and educational attainment?


