# Jonathan Schlosser
# July 28, 2021
# COVID Conspiracies in the News and Public
# Part 4 - Topic Modeling


#### Preliminary Setup ####
# Loading in libraries
require(tidyverse)
require(tidytext)
require(quanteda)
require(stm)

# Loading in data
Full_News_Data_Tidy <-  read_csv("Data/Full_News_Data_Tidy.csv")
Full_News_Articles_Tidy <-  read_csv("Data/Full_News_Articles_Tidy.csv")
Full_Tweets_Tidy <-  read_csv("Data/Full_Tweets_Tidy.csv")

# Setting Topic Levels
topic_levels = c('Topic 1', 'Topic 2', 'Topic 3', 'Topic 4', 'Topic 5', 'Topic 6', 'Topic 7', 'Topic 8',
                 'Topic 9', 'Topic 10', 'Topic 11', 'Topic 12', 'Topic 13', 'Topic 14', 'Topic 15', 
                 'Topic 16', 'Topic 17', 'Topic 18', 'Topic 19', 'Topic 20', 'Topic 21', 'Topic 22',
                 'Topic 23', 'Topic 24', 'Topic 25', 'Topic 26', 'Topic 27', 'Topic 28', 'Topic 29', 
                 'Topic 30', 'Topic 31', 'Topic 32', 'Topic 33', 'Topic 34', 'Topic 35', 'Topic 36', 
                 'Topic 37', 'Topic 38', 'Topic 39', 'Topic 40', 'Topic 41', 'Topic 42', 'Topic 43', 
                 'Topic 44', 'Topic 45', 'Topic 46', 'Topic 47', 'Topic 48', 'Topic 49', 'Topic 50', 
                 'Topic 51', 'Topic 52', 'Topic 53', 'Topic 54', 'Topic 55', 'Topic 56', 'Topic 57', 
                 'Topic 58', 'Topic 59', 'Topic 60')

# ==== MODELLING NEWS DESCRIPTIONS ====
#### Creating DFM ####
# Creating the DFM
News_Data_DFM <- Full_News_Data_Tidy %>%
  count(title, WordStem, sort = TRUE) %>%
  filter(n > 1) %>%
  cast_dfm(title, WordStem, n)

News_Data_STM = convert(News_Data_DFM, to = 'stm')


#### == STM Spectral == ####

#### Identifying Optimal K ####
# Find_K_Values_News_Data_Spec <- searchK(News_Data_STM$documents, News_Data_STM$vocab, K = c(10, 20, 30, 40, 50, 60, 70, 80),
#                                    data = News_Data_STM$meta, seed = 374075, init.type = "Spectral")
# 
# png("Plots/SpecNewsDataTopicModelDiagnostics.png", width = 1000, height = 600)
# plot(Find_K_Values_News_Data_Spec)
# dev.off()


#### Running Topic Model ####
# Decided on 50 Topics. Held-out likelihood is second highest, 
# residuals are lowest, and semantic coherence is fair. 
News_Data_Topic_Model_Spec <- stm(News_Data_DFM, K = 50, 
                             verbose = FALSE, init.type = "Spectral", seed = 374075)

#### Identifying Highest Words in Each Topic #### 
News_Data_Topics_Spec <- tidy(News_Data_Topic_Model_Spec)

News_Data_Topics_Plot_Spec <- News_Data_Topics_Spec %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()+
  scale_x_reordered()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Coronavirus Conspiracy News Descriptions")+
  guides(fill=FALSE)

png("Plots/SpecNewsDataTopicModel.png", width = 1400, height = 1000)
News_Data_Topics_Plot_Spec
dev.off()

#### Identifying the descriptions in each topic ####
News_Data_Topics_Gamma_Spec <- tidy(News_Data_Topic_Model_Spec, matrix = "gamma",                    
                               document_names = rownames(News_Data_DFM))

# Note: removed those less than 0.1
News_Data_Topics_Gamma_Plot_Spec <- News_Data_Topics_Gamma_Spec %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "News Descriptions in Each Topic",
       title = "Distribution of Descriptions Within Each Topic")+
  guides(fill=FALSE)


png("Plots/SpecNewsDataTopicModelGamma.png", width = 1000, height = 600)
News_Data_Topics_Gamma_Plot_Spec
dev.off()

#### Renaming Columns ####
colnames(News_Data_Topics_Spec) <- c("Topic", "Term", "Beta")
colnames(News_Data_Topics_Gamma_Spec) <- c("Title", "Topic", "Gamma")

#### Saving Files ####
write_csv(News_Data_Topics_Spec, "Data/News_Data_Topics_Spec.csv")
write_csv(News_Data_Topics_Gamma_Spec, "Data/News_Data_Topics_Gamma_Spec.csv")

#### == STM LDA == ####

#### Identifying Optimal K ####
# Find_K_Values_News_Data_LDA <- searchK(News_Data_STM$documents, News_Data_STM$vocab, K = c(10, 20, 30, 40, 50, 60, 70, 80),
#                                    data = News_Data_STM$meta, seed = 374075, init.type = "LDA")
# 
# png("Plots/LDANewsDataTopicModelDiagnostics.png", width = 1000, height = 600)
# plot(Find_K_Values_News_Data_LDA)
# dev.off()


#### Running Topic Model ####
# 40 Topics seems appropriate based on the Find K output. 
News_Data_Topic_Model_LDA <- stm(News_Data_DFM, K = 40, 
                             verbose = FALSE, init.type = "LDA", seed = 374075)


#### Identifying Highest Words in Each Topic ####
News_Data_Topics_LDA <- tidy(News_Data_Topic_Model_LDA)

News_Data_Topics_Plot_LDA <- News_Data_Topics_LDA %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()+
  scale_x_reordered()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Coronavirus Conspiracy News Descriptions")+
  guides(fill=FALSE)

png("Plots/LDANewsDataTopicModel.png", width = 1400, height = 1000)
News_Data_Topics_Plot_LDA
dev.off()

#### Identifying the descriptions in each topic ####
News_Data_Topics_Gamma_LDA <- tidy(News_Data_Topic_Model_LDA, matrix = "gamma",                    
                               document_names = rownames(News_Data_DFM))

# Note: removed those less than 0.1
News_Data_Topics_Gamma_Plot_LDA <- News_Data_Topics_Gamma_LDA %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "News Descriptions in Each Topic",
       title = "Distribution of Descriptions Within Each Topic")+
  guides(fill=FALSE)


png("Plots/LDANewsDataTopicModelGamma.png", width = 1000, height = 600)
News_Data_Topics_Gamma_Plot_LDA
dev.off()

#### Renaming Columns ####
colnames(News_Data_Topics_LDA) <- c("Topic", "Term", "Beta")
colnames(News_Data_Topics_Gamma_LDA) <- c("Title", "Topic", "Gamma")

#### Saving Files ####
write_csv(News_Data_Topics_LDA, "Data/News_Data_Topics_LDA.csv")
write_csv(News_Data_Topics_Gamma_LDA, "Data/News_Data_Topics_Gamma_LDA.csv")

# ==== MODELLING NEWS ARTICLES ====

#### Creating DFM ####
# Creating the DFM
News_Articles_DFM <- Full_News_Articles_Tidy %>%
  count(title, WordStem, sort = TRUE) %>%
  filter(n > 1) %>%
  cast_dfm(title, WordStem, n)

News_Articles_STM = convert(News_Articles_DFM, to = 'stm')

#### == STM Spectral == ####

#### Identifying Optimal K ####
# Find_K_Values_News_Articles_Spec <- searchK(News_Articles_STM$documents, News_Articles_STM$vocab, K = c(10, 20, 30, 40, 50, 60, 70, 80),
#                                         data = News_Articles_STM$meta, seed = 374075, init.type = "Spectral")
# 
# png("Plots/SpecNewsArticlesTopicModelDiagnostics.png", width = 1000, height = 600)
# plot(Find_K_Values_News_Articles_Spec)
# dev.off()


#### Running Topic Model ####
# Decided to go with 50 because the statistics look the best. This matches the descriptions. 
News_Articles_Topic_Model_Spec <- stm(News_Articles_DFM, K = 50, 
                                  verbose = FALSE, init.type = "Spectral", seed = 374075)

#### Identifying Highest Words in Each Topic ####
News_Articles_Topics_Spec <- tidy(News_Articles_Topic_Model_Spec)

News_Articles_Topics_Plot_Spec <- News_Articles_Topics_Spec %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()+
  scale_x_reordered()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Coronavirus Conspiracy News Articles")+
  guides(fill=FALSE)

png("Plots/SpecNewsArticlesTopicModel.png", width = 1400, height = 1000)
News_Articles_Topics_Plot_Spec
dev.off()

#### Identifying the Articles in each topic ####
News_Articles_Topics_Gamma_Spec <- tidy(News_Articles_Topic_Model_Spec, matrix = "gamma",                    
                                    document_names = rownames(News_Articles_DFM))

# Note: removed those less than 0.1
News_Articles_Topics_Gamma_Plot_Spec <- News_Articles_Topics_Gamma_Spec %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "News Articles in Each Topic",
       title = "Distribution of Articles Within Each Topic")+
  guides(fill=FALSE)


png("Plots/SpecNewsArticlesTopicModelGamma.png", width = 1000, height = 600)
News_Articles_Topics_Gamma_Plot_Spec
dev.off()

#### Renaming Columns ####
colnames(News_Articles_Topics_Spec) <- c("Topic", "Term", "Beta")
colnames(News_Articles_Topics_Gamma_Spec) <- c("Title", "Topic", "Gamma")

#### Saving Files ####
write_csv(News_Articles_Topics_Spec, "Data/News_Articles_Topics_Spec.csv")
write_csv(News_Articles_Topics_Gamma_Spec, "Data/News_Articles_Topics_Gamma_Spec.csv")

#### == STM LDA == ####

#### Identifying Optimal K ####
# Find_K_Values_News_Articles_LDA <- searchK(News_Articles_STM$documents, News_Articles_STM$vocab, K = c(10, 20, 30, 40, 50, 60, 70, 80),
#                                        data = News_Articles_STM$meta, seed = 374075, init.type = "LDA")
# 
# png("Plots/LDANewsArticlesTopicModelDiagnostics.png", width = 1000, height = 600)
# plot(Find_K_Values_News_Articles_LDA)
# dev.off()


#### Running Topic Model ####
# Decided to go with 50 because the statistics look the best. This matches the descriptions. 
News_Articles_Topic_Model_LDA <- stm(News_Articles_DFM, K = 60, 
                                 verbose = FALSE, init.type = "LDA", seed = 374075)


#### Identifying Highest Words in Each Topic ####
News_Articles_Topics_LDA <- tidy(News_Articles_Topic_Model_LDA)

News_Articles_Topics_Plot_LDA <- News_Articles_Topics_LDA %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()+
  scale_x_reordered()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Coronavirus Conspiracy News Articles")+
  guides(fill=FALSE)

png("Plots/LDANewsArticlesTopicModel.png", width = 1400, height = 1000)
News_Articles_Topics_Plot_LDA
dev.off()

#### Identifying the Articles in each topic ####
News_Articles_Topics_Gamma_LDA <- tidy(News_Articles_Topic_Model_LDA, matrix = "gamma",                    
                                   document_names = rownames(News_Articles_DFM))

# Note: removed those less than 0.1
News_Articles_Topics_Gamma_Plot_LDA <- News_Articles_Topics_Gamma_LDA %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "News Articles in Each Topic",
       title = "Distribution of Articles Within Each Topic")+
  guides(fill=FALSE)


png("Plots/LDANewsArticlesTopicModelGamma.png", width = 1000, height = 600)
News_Articles_Topics_Gamma_Plot_LDA
dev.off()

#### Renaming Columns ####
colnames(News_Articles_Topics_LDA) <- c("Topic", "Term", "Beta")
colnames(News_Articles_Topics_Gamma_LDA) <- c("Title", "Topic", "Gamma")

#### Saving Files ####
write_csv(News_Articles_Topics_LDA, "Data/News_Articles_Topics_LDA.csv")
write_csv(News_Articles_Topics_Gamma_LDA, "Data/News_Articles_Topics_Gamma_LDA.csv")


# ==== MODELLING TWEETS ====

#### Creating DFM ####
# Creating the DFM
Tweets_DFM <- Full_Tweets_Tidy %>%
  count(text, WordStem, sort = TRUE) %>%
  filter(n > 1) %>%
  cast_dfm(text, WordStem, n)

Tweets_STM = convert(Tweets_DFM, to = 'stm')

#### STM Spectral ####


#### Identifying Optimal K ####
# Find_K_Values_Tweets_Spec <- searchK(Tweets_STM$documents, Tweets_STM$vocab, K = c(10, 20, 30, 40, 50, 60, 70, 80),
#                                 data = Tweets_STM$meta, seed = 374075, init.type = "Spectral")
# 
# png("Plots/SpecTweetsTopicModelDiagnostics.png", width = 1000, height = 600)
# plot(Find_K_Values_Tweets_Spec)
# dev.off()


#### Running Topic Model ####
# Went with 50 based on the diagnostics. 
Tweets_Topic_Model_Spec <- stm(Tweets_DFM, K = 50, verbose = FALSE, init.type = "Spectral", seed = 374075)

#### Identifying Highest Words in Each Topic ####
Tweets_Topics_Spec <- tidy(Tweets_Topic_Model_Spec)

Tweets_Topics_Plot_Spec <- Tweets_Topics_Spec %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()+
  scale_x_reordered()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Coronavirus Conspiracy Tweets")+
  guides(fill=FALSE)

png("Plots/SpecTweetsTopicModel.png", width = 1400, height = 1000)
Tweets_Topics_Plot_Spec
dev.off()

#### Identifying the Episodes in each topic ####
Tweets_Gamma_Spec <- tidy(Tweets_Topic_Model_Spec, matrix = "gamma",                    
                     document_names = rownames(Tweets_DFM))

# Note: removed those less than 0.1
Tweets_Gamma_Plot_Spec <- Tweets_Gamma_Spec %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "Tweets in Each Topic",
       title = "Distribution of Tweets Within Each Topic")+
  guides(fill=FALSE)


png("Plots/SpecTweetsTopicModelGamma.png", width = 1000, height = 600)
Tweets_Gamma_Plot_Spec
dev.off()

#### Renaming Columns ####
colnames(Tweets_Topics_Spec) <- c("Topic", "Term", "Beta")
colnames(Tweets_Gamma_Spec) <- c("Tweet", "Topic", "Gamma")

#### Saving Files ####
write_csv(Tweets_Topics_Spec, "Data/Tweets_Topics_Spec.csv")
write_csv(Tweets_Gamma_Spec, "Data/Tweets_Gamma_Spec.csv")

#### == STM LDA == ####

#### Identifying Optimal K ####
# Find_K_Values_Tweets_LDA <- searchK(Tweets_STM$documents, Tweets_STM$vocab, K = c(10, 20, 30, 40, 50, 60, 70, 80),
#                                 data = Tweets_STM$meta, seed = 374075, init.type = "LDA")
# 
# png("Plots/LDATweetsTopicModelDiagnostics.png", width = 1000, height = 600)
# plot(Find_K_Values_Tweets_LDA)
# dev.off()


#### Running Topic Model ####
# Went with 40 Hours per week... 
Tweets_Topic_Model_LDA <- stm(Tweets_DFM, K = 40, verbose = FALSE, init.type = "LDA", seed = 374075)

#### Identifying Highest Words in Each Topic ####
Tweets_Topics_LDA <- tidy(Tweets_Topic_Model_LDA)

Tweets_Topics_Plot_LDA <- Tweets_Topics_LDA %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()+
  scale_x_reordered()+
  scale_fill_gradient(high = 'red', low = 'blue')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Coronavirus Conspiracy Tweets")+
  guides(fill=FALSE)

png("Plots/LDATweetsTopicModel.png", width = 1400, height = 1000)
Tweets_Topics_Plot_LDA
dev.off()

#### Identifying the Episodes in each topic ####
Tweets_Gamma_LDA <- tidy(Tweets_Topic_Model_LDA, matrix = "gamma",                    
                     document_names = rownames(Tweets_DFM))

# Note: removed those less than 0.1
Tweets_Gamma_Plot_LDA <- Tweets_Gamma_LDA %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "Tweets in Each Topic",
       title = "Distribution of Tweets Within Each Topic")+
  guides(fill=FALSE)


png("Plots/LDATweetsTopicModelGamma.png", width = 1000, height = 600)
Tweets_Gamma_Plot_LDA
dev.off()

#### Renaming Columns ####
colnames(Tweets_Topics_LDA) <- c("Topic", "Term", "Beta")
colnames(Tweets_Gamma_LDA) <- c("Tweet", "Topic", "Gamma")

#### Saving Files ####
write_csv(Tweets_Topics_LDA, "Data/Tweets_Topics_LDA.csv")
write_csv(Tweets_Gamma_LDA, "Data/Tweets_Gamma_LDA.csv")


# ==============================================================================
# ==== END OF CODE ====