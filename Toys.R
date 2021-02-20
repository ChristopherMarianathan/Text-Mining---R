##################################################################
###############    Importing Libraries ###########################
##################################################################
library(wordcloud)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(textdata)
library(gutenbergr)
library(reshape2)
##################################################################
############### Downloading books##### ###########################
##################################################################
alice<- gutenberg_works(title == "Alice's Adventures in Wonderland") %>%
  gutenberg_download(strip = FALSE,mirror="http://mirrors.xmission.com/gutenberg/")
oz<- gutenberg_works(title == "The Wonderful Wizard of Oz") %>%
  gutenberg_download(strip = FALSE,mirror="http://mirrors.xmission.com/gutenberg/")

jungle<- gutenberg_works(title == "The Jungle Book") %>%
  gutenberg_download(strip = FALSE,mirror="http://mirrors.xmission.com/gutenberg/")
##################################################################
############### Tidying books ####################################
##################################################################
data(stop_words) 
tidy_alice <- alice %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_oz <- oz %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_jungle <- jungle %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

##################################################################
############### Combining all books to find frequency#############
##################################################################
frequency <- bind_rows(mutate(tidy_alice, book="Alice in Wonderland"),
                       mutate(tidy_oz, book= "Wizard of Oz"),
                       mutate(tidy_jungle, book="Jungle Book")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(book, word) %>%
  group_by(book)%>%
  mutate(proportion = n/sum(n))%>%
  select(-n)  %>%
  spread(book, proportion) %>%
  gather(book, proportion, "Wizard of Oz", "Jungle Book")
##################################################################
############### Plotting Correlogram #############################
##################################################################
ggplot(frequency, aes(x=proportion, y=`Alice in Wonderland`, 
                      color = abs(`Alice in Wonderland`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~book, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Alice in Wonderland", x=NULL)


##########################################
##doing the cor.test() ###################
##########################################
cor.test(data=frequency[frequency$book == "Jungle Book",],
         ~proportion + `Alice in Wonderland`)
cor.test(data=frequency[frequency$book == "Wizard of Oz",],
         ~proportion + `Alice in Wonderland`)
#################################################################
### Identifying common word counts among texts###################
#################################################################
original_books_t <- bind_rows(mutate(tidy_alice, book="Alice in Wonderland"),
                              mutate(tidy_oz, book= "Wizard of Oz"),
                              mutate(tidy_jungle, book="Jungle Book")) %>%
  count(book, word, sort=TRUE) %>%
  ungroup()
total_words_t <- original_books_t %>%
  group_by(book) %>%
  summarize(total= sum(n))

book_words_t <- left_join(original_books_t, total_words_t)

print(book_words_t)

######################################
########## ZIPF's law ################
######################################
freq_by_rank_t <- book_words_t %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank_t

#let's plot ZIPF's Law
freq_by_rank_t %>%
  ggplot(aes(rank, `term frequency`, color=book))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()
###################################################
################# TF_IDF ##########################
###################################################
book_words_t <- book_words_t %>%
  bind_tf_idf(word, book, n)

book_words_t # we get all the zeors because we are looking at stop words ... too common
book_words_t %>%
  arrange(desc(tf_idf))
#what can we say about these words?
# looking at the graphical apprach:
book_words_t %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(30) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=book))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~book, ncol=2, scales="free")+
  coord_flip()
############################################
##### Creating a word cloud for texts######
###########################################

original_books_tn <- bind_rows(mutate(tidy_alice, book="Alice in Wonderland"),
                               mutate(tidy_oz, book= "Wizard of Oz"),
                               mutate(tidy_jungle, book="Jungle Book")) %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(word, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE))))%>%
  ungroup() %>%
  filter(book == 'Alice in Wonderland') %>%
  count(word, sort=T)
original_books_tn %>%
  with(wordcloud(word, n, max.words = 100)) 
###################################################
#### Adding positive and negative sentiments ######
###################################################
original_books_tn %>%
  inner_join(get_sentiments('nrc')) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var='n', fill=0) %>%
  comparison.cloud(colors = c('grey20', 'grey80'),
                   max.words=500,scale=c(1,0.1))
original_books_tn %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var='n', fill=0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   max.words=500,scale=c(1,0.1))

######################################################################
################# Creation of bigrams ################################
######################################################################
toy_bigrams <- bind_rows(mutate(alice, book="Alice in Wonderland"),
                         mutate(oz, book= "Wizard of Oz"),
                         mutate(jungle, book="Jungle Book")) %>%
  unnest_tokens(bigram, text, token = 'ngrams', n=2)
toy_bigrams_separated <- toy_bigrams %>%
  separate(bigram, c('word1', 'word2'), sep = " ")
toy_bigrams_filtered <- toy_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 == 'project')%>%
  filter(!word1 == 'gutenberg')%>%
  filter(!word1 == 'tm')%>%
  filter(!word1 == 'archive')%>%
  filter(!word1 == 'literary')%>%
  filter(!word1 == 'NA' | !word2 == 'NA')
toy_bigram_counts <- toy_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
toy_bigram_counts
######################################################################
################# Creation of trigrams ################################
######################################################################
toy_trigram <- bind_rows(mutate(alice, book="Alice in Wonderland"),
                         mutate(oz, book= "Wizard of Oz"),
                         mutate(jungle, book="Jungle Book")) %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  separate(trigram, c('word1', "word2", 'word3'), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)%>%
  filter(!word1 == 'project')%>%
  filter(!word1 == 'gutenberg')%>%
  filter(!word1 == 'tm')%>%
  filter(!word1 == 'archive')%>%
  filter(!word1 == 'literary')%>%
  filter(!word1 == 'title')%>%
  filter(!word1 == 'author')%>%
  filter(!word1 == 'release')%>%
  filter(!word1 == 'date')%>%
  filter(!word1 == 'june')%>%
  filter(!word1 == '25')%>%
  filter(!word1 == '2008')%>%
  filter(!word1 == 'recently')%>%
  filter(!word1 == 'updated')%>%
  filter(!word1 == 'october')%>%
  filter(!word1 == 'character')%>%
  filter(!word1 == 'set')%>%
  filter(!word1 == 'encoding')%>%
  filter(!word1 == 'millennium')%>%
  filter(!word1 == 'fulcrum')%>%
  filter(!word1 == 'ebook')%>%
  filter(!word1 == 'NA' | !word2 == 'NA')
toy_trigram
###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and trigram #################
###########################################################
# TF-IDF for bigrams
toy_bigram_united <- toy_bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
toy_bigram_tf_idf <- toy_bigram_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
toy_bigram_tf_idf
# TF-IDF for trigrams
toy_trigram_united <- toy_trigram %>%
  unite(trigram, word1, word2, word3, sep=" ") #we need to unite what we split in the previous section
toy_trigram_tf_idf <- toy_trigram_united %>%
  count(book, trigram) %>%
  bind_tf_idf(trigram, book, n) %>%
  arrange(desc(tf_idf))
toy_trigram_tf_idf













