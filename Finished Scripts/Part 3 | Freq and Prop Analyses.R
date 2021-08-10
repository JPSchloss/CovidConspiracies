# Jonathan Schlosser
# July 28, 2021
# COVID Conspiracies in the News and Public
# Part 3 - Count and Proportion Analyses


#### Preliminary Setup ####
# Loading Libraries
require(tidyverse)
require(tidytext)
require(gridExtra)

#Loading in data
Full_News_Data = read.csv("Data/Full_News_Data.csv", encoding="UTF-8", stringsAsFactors=FALSE)
Full_News_Articles = read.csv("Data/Full_News_Articles.csv", encoding="UTF-8", stringsAsFactors=FALSE)
Full_Tweets = read.csv("Data/Covid_Tweets.csv", encoding="UTF-8", stringsAsFactors=FALSE)
Full_News_Data_Tidy = read_csv("Data/Full_News_Data_Tidy.csv")
Full_News_Articles_Tidy = read_csv("Data/Full_News_Articles_Tidy.csv")
Full_Tweets_Tidy = read_csv("Data/Full_Tweets_Tidy.csv")


# ==== DOCUMENTS OVER TIME ====
#Setting up the date comlumn. 
Full_News_Data$working_date = as.Date(Full_News_Data$published_at)
Full_News_Articles$working_date = as.Date(Full_News_Articles$date)
Full_Tweets$working_date = as.Date(Full_Tweets$created_at)

# Dataframes of Count of Docs Over Time 
Full_News_Data_wf = Full_News_Data %>%
  group_by(working_date) %>%
  count(working_date)

Full_News_Articles_wf = Full_News_Articles %>%
  group_by(working_date) %>%
  count(working_date)

Full_Tweets_wf = Full_Tweets %>%
  group_by(working_date) %>%
  count(working_date)

# Merging Dataframes Together
timecount_wf = merge(Full_News_Data_wf, Full_News_Articles_wf,  by = 'working_date', all = TRUE)
timecount_wf = merge(timecount_wf, Full_Tweets_wf,  by = 'working_date', all = TRUE)

# Setting cleaner column names
colnames(timecount_wf) = c('working_date', 'news', 'articles', 'tweets')

# Creating Discriptions Document Count Plot
descriptions_timecount = ggplot(timecount_wf, aes(x = working_date)) +
  geom_line(aes(y = news), color = 'red') +
  scale_x_date(labels = date_format("%m/%d"), date_breaks = '5 day') +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Date", y = "Document Frequency",
       title = "Count of News Descriptions Per Day")+
  guides(fill=FALSE)

png("Plots/DescriptionFrequencyPerDay.png", width = 1000, height = 600)
descriptions_timecount
dev.off()

# Creating Articles Document Count Plot
articles_timecount = ggplot(timecount_wf, aes(x = working_date)) +
  geom_line(aes(y = articles), color = 'black') +
  scale_x_date(labels = date_format("%m/%d"), date_breaks = '5 day') +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Date", y = "Document Frequency",
       title = "Count of News Articles Per Day")+
  guides(fill=FALSE)

png("Plots/ArticleFrequencyPerDay.png", width = 1000, height = 600)
articles_timecount
dev.off()

# Creating Tweets Document Count Plot
tweets_timecount = ggplot(timecount_wf, aes(x = working_date)) +
  geom_line(aes(y = tweets), color = 'blue') +
  scale_x_date(labels = date_format("%m/%d"), date_breaks = '5 day') +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Date", y = "Document Frequency",
       title = "Count of Tweets Per Day")+
  guides(fill=FALSE)

png("Plots/TweetFrequencyPerDay.png", width = 1000, height = 600)
tweets_timecount
dev.off()


# ==== SETTING UP COUNT AND PROPORTION DATAFRAMES ====

# News Descriptions Dataframe
Stem_News_Data_Props  <-  Full_News_Data_Tidy %>%
  count(WordStem) %>%
  filter(n > 1) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

colnames(Stem_News_Data_Props) = c('WordStem', 'Count', 'Proportion')

write_csv(Stem_News_Data_Props, "Data/Stem_News_Data_Props.csv")

# News Articles Dataframe
Stem_News_Article_Props  <-  Full_News_Articles_Tidy %>%
  count(WordStem) %>%
  filter(n > 1) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

colnames(Stem_News_Article_Props) = c('WordStem', 'Count', 'Proportion')

write_csv(Stem_News_Article_Props, "Data/Stem_News_Article_Props.csv")

# Tweet Dataframe
Stem_Tweets_Props  <-  Full_Tweets_Tidy %>%
  count(WordStem) %>%
  filter(n > 1) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

colnames(Stem_Tweets_Props) = c('WordStem', 'Count', 'Proportion')

write_csv(Stem_Tweets_Props, "Data/Stem_Tweets_Props.csv")


# ==== TERM FREQUENCY ANALYSES ====

# Top 50 Word Count Descriptions
Stem_News_Data_Top_50_Count <- ggplot(Stem_News_Data_Props[1:50,], aes(x=reorder(WordStem, n), y=n)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Stemmed Words", y = "Term Frequency",
       title = "Count of Stemmed Terms in \nCoronavirus Conspiracy News Descriptions")+
  guides(fill=FALSE)


png("Plots/StemTop50NewsData.png", width = 600, height = 800)
Stem_News_Data_Top_50_Count
dev.off()


# Top 50 Word Count Articles
Stem_News_Article_Top_50_Count <- ggplot(Stem_News_Article_Props[1:50,], aes(x=reorder(WordStem, n), y=n)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Stemmed Words", y = "Term Frequency",
       title = "Count of Stemmed Terms in \nCoronavirus Conspiracy News Articles")+
  guides(fill=FALSE)


png("Plots/StemTop50NewsArticles.png", width = 600, height = 800)
Stem_News_Article_Top_50_Count
dev.off()


# Top 50 Word Count Tweets
Stem_Tweets_Top_50_Count <- ggplot(Stem_Tweets_Props[1:50,], aes(x=reorder(WordStem, n), y=n)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Stemmed Words", y = "Term Frequency",
       title = "Count of Stemmed Terms in \nCoronavirus Conspiracy Tweets")+
  guides(fill=FALSE)


png("Plots/StemTop50Tweets.png", width = 600, height = 800)
Stem_Tweets_Top_50_Count
dev.off()


# ==== FREQUENCY SUMMARY STATISTICS ====
# Descriptions
Overall_Stem_News_Data_Count_Stats <- Stem_News_Data_Props %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("Tables/OverallStemNewsDataStats.png", width = 480, height = 480, bg = "white")
grid.table(Overall_Stem_News_Data_Count_Stats)
dev.off()

# Articles
Overall_Stem_News_Article_Count_Stats <- Stem_News_Article_Props %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("Tables/OverallStemNewsArticleStats.png", width = 480, height = 480, bg = "white")
grid.table(Overall_Stem_News_Article_Count_Stats)
dev.off()

# Tweets
Overall_Stem_Tweets_Count_Stats <- Stem_Tweets_Props %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("Tables/OverallStemTweetStats.png", width = 480, height = 480, bg = "white")
grid.table(Overall_Stem_Tweets_Count_Stats)
dev.off()


# ==== PROPORTION ANALYSES ====

# Top 50 Word Prop Descriptions
Stem_News_Data_Top_50_Props <- ggplot(Stem_News_Data_Props[1:50,], aes(x=reorder(WordStem, prop), y=prop)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Stemmed Words", y = "Term Proportion",
       title = "Proportion of Times Stemmed Word Appears in \nCoronavirus Conspiracy Descriptions")+
  guides(fill=FALSE)


png("Plots/Top50NewsDataProportions.png", width = 600, height = 800)
Stem_News_Data_Top_50_Props
dev.off()

# Top 50 Word Prop Articles
Stem_News_Article_Top_50_Props <- ggplot(Stem_News_Article_Props[1:50,], aes(x=reorder(WordStem, prop), y=prop)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Stemmed Words", y = "Term Proportion",
       title = "Proportion of Times Stemmed Word Appears in \nCoronavirus Conspiracy Articles")+
  guides(fill=FALSE)


png("Plots/Top50NewsArticlesProportions.png", width = 600, height = 800)
Stem_News_Article_Top_50_Props
dev.off()


# Top 50 Word Prop Tweets
Stem_Tweets_Top_50_Props <- ggplot(Stem_Tweets_Props[1:50,], aes(x=reorder(WordStem, prop), y=prop)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Stemmed Words", y = "Term Proportion",
       title = "Proportion of Times Stemmed Word Appears in \nCoronavirus Conspiracy Tweets")+
  guides(fill=FALSE)


png("Plots/Top50NewsTweetsProportions.png", width = 600, height = 800)
Stem_Tweets_Top_50_Props
dev.off()




# ==== PROPORTION SUMMARY STATISTICS ====
# Descriptions
Overall_Stem_News_Data_Prop_Stats <- Stem_News_Data_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop))  

png("Tables/OverallStemNewsDataPropStats.png", width = 480, height = 480, bg = "white")
grid.table(Overall_Stem_News_Data_Prop_Stats)
dev.off()

# Articles
Overall_Stem_News_Article_Prop_Stats <- Stem_News_Article_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop))  

png("Tables/OverallStemNewsArticlePropStats.png", width = 480, height = 480, bg = "white")
grid.table(Overall_Stem_News_Article_Prop_Stats)
dev.off()

# Tweets
Overall_Stem_Tweets_Prop_Stats <- Stem_Tweets_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop))  

png("Tables/OverallStemTweetPropStats.png", width = 480, height = 480, bg = "white")
grid.table(Overall_Stem_Tweets_Prop_Stats)
dev.off()


# ==== TFIDF ANALYSES - Not Running - Skipped Over ====
# Calculating TF-IDF For News Descriptions
Stem_News_Data_tfidf <- Full_News_Data_Tidy %>%
  count(WordStem, title) %>%
  filter(n > 1) %>%
  bind_tf_idf(WordStem, title, n) 

Stem_Top_15_News_Data_TFIDF <- Stem_News_Data_tfidf %>%
  group_by(title) %>%
  arrange(title, -tf_idf) %>%
  top_n(15)

# Calculating TF-IDF For News Articles
Stem_News_Articles_tfidf <- Full_News_Articles_Tidy %>%
  count(WordStem, title) %>%
  filter(n > 1) %>%
  bind_tf_idf(WordStem, title, n) 

Stem_Top_15_News_Articles_TFIDF <- Stem_News_Articles_tfidf %>%
  group_by(title) %>%
  arrange(title, -tf_idf) %>%
  top_n(15)

# Calculating TF-IDF For Tweets
Stem_Tweets_Data_tfidf <- Full_Tweets_Tidy %>%
  count(WordStem, text) %>%
  filter(n > 1) %>%
  bind_tf_idf(WordStem, text, n) 

Stem_Top_15_Tweets_Data_TFIDF <- Stem_Tweets_Data_tfidf %>%
  group_by(text) %>%
  arrange(text, -tf_idf) %>%
  top_n(15)

# Saving TF-IDF Files
write.csv(Stem_News_Data_tfidf, "Data/Descriptions_TFIDF.csv", row.names = FALSE)
write.csv(Stem_News_Articles_tfidf, "Data/Articles_TFIDF.csv", row.names = FALSE)
write.csv(Stem_Tweets_Data_tfidf, "Data/Tweets_TFIDF.csv", row.names = FALSE)
# ==============================================================================
# ==== END OF CODE ====

