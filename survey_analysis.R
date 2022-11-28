# R Survey analysis

#### Read in packages and files ####

# Standard text mining package
#library(limonade)
library(tm) # https://www.r-bloggers.com/2021/05/sentiment-analysis-in-r-3/
library(deeplr) # For translating French surveys
library(readr)


DEEPL_API_KEY = readLines("credentials/deepl_api_key.txt")

raw_survey  = read_csv("raw_data/results-survey961781.csv")
#raw_survey_fr  = read_csv("raw_data/results-survey961781_fr.csv")


#### Text mining ####

#### Clustering #####

#### Score analysis ####
