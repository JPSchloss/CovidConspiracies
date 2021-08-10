# Jonathan Schlosser
# July 28, 2021
# COVID Conspiracies in the News and Public
# Part 5 - Topic Model Time Series Wrangling

#### Preliminary Setup ####
#Loading in libraries
library(tidyverse)
library(tidytext)

#Loading in data
Full_News_Data_Tidy <-  read_csv("Data/Full_News_Data_Tidy.csv")
News_Data_Topics_Spec <-  read_csv("Data/News_Data_Topics_Spec.csv")
News_Data_Topics_Gamma_Spec <-  read_csv("Data/News_Data_Topics_Gamma_Spec.csv")
News_Data_Topics_LDA <-  read_csv("Data/News_Data_Topics_LDA.csv")
News_Data_Topics_Gamma_LDA <-  read_csv("Data/News_Data_Topics_Gamma_LDA.csv")


Full_News_Articles_Tidy <-  read_csv("Data/Full_News_Articles_Tidy.csv")
News_Articles_Topics_Spec <-  read_csv("Data/News_Articles_Topics_Spec.csv")
News_Articles_Topics_Gamma_Spec <-  read_csv("Data/News_Articles_Topics_Gamma_Spec.csv")
News_Articles_Topics_LDA <-  read_csv("Data/News_Articles_Topics_LDA.csv")
News_Articles_Topics_Gamma_LDA <-  read_csv( "Data/News_Articles_Topics_Gamma_LDA.csv")


Full_Tweets_Tidy <-  read_csv("Data/Full_Tweets_Tidy.csv")
Tweets_Topics_Spec <-  read_csv("Data/Tweets_Topics_Spec.csv")
Tweets_Gamma_Spec <-  read_csv("Data/Tweets_Gamma_Spec.csv")
Tweets_Topics_LDA <-  read_csv("Data/Tweets_Topics_LDA.csv")
Tweets_Gamma_LDA <-  read_csv("Data/Tweets_Gamma_LDA.csv")


#### ==== PRELIMINARY DATAFRAMES ==== ####
# Descriptions
Description_Dates <- Full_News_Data_Tidy %>%
  select(title, description, name, published_at) %>%
  distinct()

colnames(Description_Dates) <- c("Title", "Description", "NewsSource", "Date")

write_csv(Description_Dates, "Data/Description_Dates.csv")
Description_Dates <- read_csv("Data/Description_Dates.csv")

# Articles
Article_Dates <- Full_News_Articles_Tidy %>%
  select(title, article, source, date) %>%
  distinct()

colnames(Article_Dates) <- c("Title", "Article", "NewsSource", "Date")

write_csv(Article_Dates, "Data/Article_Dates.csv")
Article_Dates <- read_csv("Data/Article_Dates.csv")

# Tweets
Tweet_Dates <- Full_Tweets_Tidy %>%
  select(text, created_at) %>%
  distinct()

colnames(Tweet_Dates) <- c("Tweet", "Date")

Tweet_Dates$WorkingTweet = str_sub(Tweet_Dates$Tweet, 1, 60)

write_csv(Tweet_Dates, "Data/Tweet_Dates.csv")
Tweet_Dates <- read_csv("Data/Tweet_Dates.csv")

#### ==== TERMS AND TIMES ==== #### 
#### Descriptions term and time ####
Descriptions_Word_Time <- Full_News_Data_Tidy %>%
  select(WordStem, published_at)
#32282 Observations

colnames(Descriptions_Word_Time) = c("WordStem", "Date")

Descriptions_Word_Time <- Descriptions_Word_Time %>%
  count(WordStem, sort = TRUE) %>%
  filter(n > 5) %>%
  inner_join(Descriptions_Word_Time) %>%
  select(-n)

write_csv(Descriptions_Word_Time, "Data/Descriptions_Word_Time.csv")

#### Articles term and time ####
Articles_Word_Time <- Full_News_Articles_Tidy %>%
  select(WordStem, date)
# 1388067 Observations

colnames(Articles_Word_Time) = c("WordStem", "Date")

Articles_Word_Time <- Articles_Word_Time %>%
  count(WordStem, sort = TRUE) %>%
  filter(n > 20) %>%
  inner_join(Articles_Word_Time) %>%
  select(-n)

write_csv(Articles_Word_Time, "Data/Articles_Word_Time.csv")

#### Tweet term and time ####
Tweet_Word_Time <- Full_Tweets_Tidy %>%
  select(WordStem, created_at)
# 5,647,607 Observations

colnames(Tweet_Word_Time) = c("WordStem", "Date")

Tweet_Word_Time <- Tweet_Word_Time %>%
  count(WordStem, sort = TRUE) %>%
  filter(n > 20) %>%
  inner_join(Tweet_Word_Time) %>%
  select(-n)

write_csv(Tweet_Word_Time, "Data/Tweet_Word_Time.csv")


#### ==== TOPICS AND TIMES ====
#### Merging Descriptions and Time ####
Spec_Time_Descriptions <- News_Data_Topics_Gamma_Spec %>%
  left_join(Description_Dates, by = "Title")

Spec_Time_Descriptions$Model = 'STM'

LDA_Time_Descriptions <- News_Data_Topics_Gamma_LDA %>%
  left_join(Description_Dates, by = "Title")

LDA_Time_Descriptions$Model = 'LDA'

Time_Descriptions <- bind_rows(Spec_Time_Descriptions, LDA_Time_Descriptions)

Time_Descriptions <- Time_Descriptions %>%
  filter(Gamma > 0.01)
  
write_csv(Time_Descriptions, "Data/Time_Descriptions.csv")

#### Merging Articles and Time ####
Spec_Time_Articles <- News_Articles_Topics_Gamma_Spec %>%
  left_join(Article_Dates, by = "Title")

Spec_Time_Articles$Model = 'STM'

LDA_Time_Articles <- News_Articles_Topics_Gamma_LDA %>%
  left_join(Article_Dates, by = "Title")

LDA_Time_Articles$Model = 'LDA'

Time_Articles <- bind_rows(Spec_Time_Articles, LDA_Time_Articles)

Time_Articles <- Time_Articles %>%
  filter(Gamma > 0.01)
  
write_csv(Time_Articles, "Data/Time_Articles.csv")

#### Merging Tweets and Time ####
Spec_Time_Tweets <- Tweets_Gamma_Spec %>%
  left_join(Tweet_Dates, by = "Tweet")

Spec_Time_Tweets$Model = 'STM'

LDA_Time_Tweets <- Tweets_Gamma_LDA %>%
  left_join(Tweet_Dates, by = "Tweet")

LDA_Time_Tweets$Model = 'LDA'

Time_Tweets <- bind_rows(Spec_Time_Tweets, LDA_Time_Tweets)

Time_Tweets <- Time_Tweets %>%
  filter(Gamma > 0.02)

Time_Tweets$Date = as.Date(Time_Tweets$Date)

Time_Tweets <- Time_Tweets %>%
  group_by(Model, Topic, Date) %>%
  arrange(-Gamma) %>%
  top_n(1) %>%
  arrange(Model, Topic)

Time_Tweets <- Time_Tweets %>%
  select(-c(Tweet, WorkingTweet))
  
write_csv(Time_Tweets, "Data/Time_Tweets.csv")

#### ==== BETA AND GAMMA DATA WRANGLING ====
#### Merging Description Betas ####

News_Data_Topics_Spec$Model = 'STM'
News_Data_Topics_LDA$Model = 'LDA'

News_Data_Topics <- bind_rows(News_Data_Topics_Spec, News_Data_Topics_LDA)
# 92430 Observations

News_Data_Topics <- News_Data_Topics %>%
  group_by(Model, Topic) %>%
  slice_max(Beta, n = 100)

write_csv(News_Data_Topics, "Data/News_Data_Topics.csv")

#### Merging Description Gammas ####

News_Data_Topics_Gamma_Spec$Model = 'STM'
News_Data_Topics_Gamma_LDA$Model = 'LDA'

News_Data_Topics_Gamma <- bind_rows(News_Data_Topics_Gamma_Spec, News_Data_Topics_Gamma_LDA)
#52830 Observations

News_Data_Topics_Gamma <- News_Data_Topics_Gamma %>%
  group_by(Model, Topic) %>%
  slice_max(Gamma, n = 100)

write_csv(News_Data_Topics_Gamma, "Data/News_Data_Topics_Gamma.csv")

#### Merging Article Betas ####

News_Articles_Topics_Spec$Model = 'STM' 
News_Articles_Topics_LDA$Model = 'LDA' 

News_Articles_Topics <- bind_rows(News_Articles_Topics_Spec, News_Articles_Topics_LDA)
# 1711710 Observations

News_Articles_Topics <- News_Articles_Topics %>%
  group_by(Model, Topic) %>%
  slice_max(Beta, n = 100)

write_csv(News_Articles_Topics, "Data/News_Articles_Topics.csv")

#### Merging Article Gammas ####
News_Articles_Topics_Gamma_Spec$Model = 'STM'
News_Articles_Topics_Gamma_LDA$Model = 'LDA'

News_Articles_Topics_Gamma <- bind_rows(News_Articles_Topics_Gamma_Spec, News_Articles_Topics_Gamma_LDA)
# 281820 Observations

News_Articles_Topics_Gamma <- News_Articles_Topics_Gamma %>%
  group_by(Model, Topic) %>%
  slice_max(Gamma, n = 100)

write_csv(News_Articles_Topics_Gamma, "Data/News_Articles_Topics_Gamma.csv")

#### Merging Tweet Betas ####

Tweets_Topics_Spec$Model = 'STM' 
Tweets_Topics_LDA$Model = 'LDA'

Tweets_Topics <- bind_rows(Tweets_Topics_Spec, Tweets_Topics_LDA)
# 1142190 Observations

Tweets_Topics <- Tweets_Topics %>%
  group_by(Model, Topic) %>%
  slice_max(Beta, n = 100)

write_csv(Tweets_Topics, "Data/Tweets_Topics.csv")

#### Merging Tweet Gammas ####

Tweets_Gamma_Spec$Model = 'STM'
Tweets_Gamma_LDA$Model = 'LDA'

Tweets_Gamma <- bind_rows(Tweets_Gamma_Spec, Tweets_Gamma_LDA)
# 13424040 Observations

Tweets_Gamma <- Tweets_Gamma %>%
  group_by(Model, Topic) %>%
  slice_max(Gamma, n = 100)

Tweets_Gamma$Working_Tweet <- str_sub(Tweets_Gamma$Tweet, 1, 60)

write_csv(Tweets_Gamma, "Data/Tweets_Gamma.csv")



#### ==== COUNT AND PROP DATA WRANGLING ====
