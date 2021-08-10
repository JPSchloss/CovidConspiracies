# Jonathan Schlosser
# July 28, 2021
# COVID Conspiracies in the News and Public
# Part 2 - Data Cleaning


#### Preliminary Setup ####
# Loading Libraries
require(tidyverse)
require(tidytext)
require(SnowballC)
require(textstem)
require(scales)

# Loading In Data
Full_News_Data = read.csv("Data/Full_News_Data.csv", encoding="UTF-8", stringsAsFactors=FALSE)
Full_News_Articles = read.csv("Data/Full_News_Articles.csv", encoding="UTF-8", stringsAsFactors=FALSE)
Full_Tweets = read.csv("Data/Covid_Tweets.csv", encoding="UTF-8", stringsAsFactors=FALSE)

# Setting up regex to clean documents
remove_regex = c("[[:upper:]]{2,}",
                 "@[A-z0-9]+\\b",
                 "[[:punct:]]",
                 "[[:digit:]]",
                 "[^\x01-\x7F]",
                 '^[0-9]\\w+')

# Setting up additional stopwords to remove
# Kept key common words to help build more accurate models 
# (https://blogs.cornell.edu/cornellnlp/2019/02/09/choose-your-words-wisely-for-topic-models/) 
remove_terms_list <- as.vector(c("ap",
                                 "copyright",
                                 "associated",
                                 "press",
                                 "min",
                                 "read",
                                 "share this with email facebook messenger messenger twitter pinterest whatsapp linkedin",
                                 "copy this link\nthese are external links and will open in a new window",
                                 "sign up here for our daily coronavirus newsletter on what you need to know",
                                 "and subscribe to our covid podcast for the latest news and analysis",
                                 "business insider",
                                 'watch cbsn live', 
                                 'updated on',
                                 'cbs interactive inc all rights reserved',
                                 "this material may not be published broadcast rewritten",
                                 "or redistributed",
                                 "fox news network llc",
                                 "all rights reserved",
                                 "all market data delayed minutes",
                                 "get all the stories you needtoknow from the most powerful name in news delivered first thing every morning to your inbox",
                                 "subscribed",
                                 "youve successfully subscribed to this newsletter",
                                 "log in using your social network account",
                                 "log in directly with the independent",
                                 "want an ad-free experience",
                                 "subscribe to independent premium",
                                 "you can find our community guidelines in full here",
                                 "create a commenting name to join the debate",
                                 "enter your email to follow new comments on this article",
                                 "are you sure you want to submit this vote",
                                 "are you sure you want to mark this comment as inappropriate",
                                 "want to discuss real-world problems, be involved in the most engaging discussions",
                                 "and hear from the journalists",
                                 "start your independent premium subscription today",
                                 "are you sure you want to delete this comment",
                                 "independent premium comments can be posted by members of our membership scheme",
                                 "independent premium",
                                 "nbc universal",
                                 "nbcnewscom",
                                 "newsweek",
                                 "politico llc",
                                 "reuters",
                                 "capitol hill publishing corp",
                                 "a subsidiary of news communications inc",
                                 "sign in",
                                 "manage newsletters",
                                 "copyright",
                                 "the washington times llc",
                                 "click here for reprint permission",
                                 "click to read more",
                                 "view comments",
                                 "click to hide",
                                 "terms of use",
                                 "privacy policy",
                                 "manage newsletters",
                                 "usa today",
                                 "t.co",
                                 "http[A-z0-9]+\\b",
                                 "it's",
                                 "ap",
                                 "amp",
                                 "people",
                                 "dont",
                                 "time",
                                 # Frequent Terms
                                 "coronavirus",
                                 "pandemic",
                                 "trump",
                                 "covid",
                                 "fake",
                                 "conspiracy",
                                 "lie",
                                 "misinformation",
                                 "spread",
                                 "test",
                                 "virus"))




# ==== CLEANING NEWS DESCRIPTIONS ====

# Establishing a clean text column
Full_News_Data$clean_text = Full_News_Data$description

# Removing Regex
for (i in remove_regex) {
  tryCatch({
    Full_News_Data$clean_text = gsub(i, "", Full_News_Data$clean_text)
  }, error=function(e){})
}

# Unnesting Words, Creating a Tidy Dataset, Removing Stop Words
Full_News_Data_Tidy <- Full_News_Data %>%
  unnest_tokens(word, clean_text, drop = FALSE) %>%
  anti_join(stop_words) %>%
  filter(!word %in% remove_terms_list)%>%
  filter(str_length(word) > 2) %>%
  mutate(WordStem = wordStem(word))
# 34,373 Observations

# Slimming Down Dataset by removing unnecessary columns
Full_News_Data_Tidy = Full_News_Data_Tidy %>%
  select(-c("author", "url_to_image", "id"))

# Kept all terms as there is little repetition among descriptions and 
# because it can affect the final model. 

# ==== CLEANING NEWS ARTICLES ====

# Establishing a clean text column
Full_News_Articles$clean_text = Full_News_Articles$article

# Removing Regex
for (i in remove_regex) {
  tryCatch({
    Full_News_Articles$clean_text = gsub(i, "", Full_News_Articles$clean_text)
  }, error=function(e){})
}

# Unnesting Words, Creating a Tidy Dataset, Removing Stop Words
Full_News_Articles_Tidy <- Full_News_Articles %>%
  unnest_tokens(word, clean_text, drop = FALSE) %>%
  anti_join(stop_words) %>%
  filter(!word %in% remove_terms_list)%>%
  filter(str_length(word) > 2) %>%
  mutate(WordStem = wordStem(word)) 
#1430901 Observations

# Kept all terms as there is little repetition among articles and 
# because it can affect the final model. 

# ==== CLEANING TWEETS ====

# Sampling the dataset to make it more workable. 
# When the full dataset is exploded there are 12,879,631 Observations. 
# This makes the dataset unworkable as the data is too large. 
# To resolve this problem the data set it reduced to about half. 

Full_Tweets = sample_n(Full_Tweets, 544765)

# Establishing a clean text column
Full_Tweets$clean_text = Full_Tweets$text

# Removing Regex
for (i in remove_regex) {
  tryCatch({
    Full_Tweets$clean_text = gsub(i, "", Full_Tweets$clean_text)
  }, error=function(e){})
}

# Unnesting Words, Creating a Tidy Dataset, Removing Stop Words
Full_Tweets_Tidy <- Full_Tweets %>%
  unnest_tokens(word, clean_text, drop = FALSE) %>%
  anti_join(stop_words) %>%
  filter(!word %in% remove_terms_list)%>%
  filter(str_length(word) > 2) %>%
  filter(str_length(word) <= 14) %>%
  mutate(WordStem = wordStem(word)) 
# 6437279 Observations

# ==== SAVING FILES ====
write.csv(Full_News_Data_Tidy, "Data/Full_News_Data_Tidy.csv", row.names = FALSE)
write.csv(Full_News_Articles_Tidy, "Data/Full_News_Articles_Tidy.csv", row.names = FALSE)
write.csv(Full_Tweets_Tidy, "Data/Full_Tweets_Tidy.csv", row.names = FALSE)
# ==============================================================================
# ==== END OF CODE ====