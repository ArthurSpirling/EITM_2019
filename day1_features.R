
#7/5/2018
#Day 1 of EITM

rm(list=ls())

#need to install() these if not yet present

require(quanteda)
require(readtext)
require(preText)
require(stm)

#lets grab ~24 House Bills as data
rt <- readtext("house_bills/*")
my.corpus <- corpus(rt)

#easy to play with the options here
my.tokens <- tokens(my.corpus, remove_numbers=F, remove_punc=T)
my.dfm <- dfm(my.tokens, tolower=F, remove = c(stopwords("english")), stem = TRUE)
#my.dfm <- dfm(my.tokens, tolower=F, remove = c(stopwords("english"), "shall", "section"), stem = F)

# keep only words occurring >= 25 times and in >= 2 documents
my.trimmed <- dfm_trim(my.dfm, min_termfreq = 25, min_docfreq = 2 )

#pass this to stm()
trim.STM <- convert(my.trimmed, to = "stm")
stm.fit <- stm(trim.STM$documents, trim.STM$vocab, K = 20)

#take a look at the summaries -- e.g.
# plot(stm.fit, type="summary") # gives key words for topics
# plot(stm.fit, type="labels")
# plot(stm.fit, type="perspectives", topics=c(1,2))
# plot(stm.fit, type="hist")

#Using preText
# see: http://www.mjdenny.com/getting_started_with_preText.html

# use first 10 documents for example
documents <- my.corpus[1:10]
# take a look at the document names
print(names(documents))

#let's get all the possible options!
preprocessed_documents <- factorial_preprocessing(
  documents,
  use_ngrams = TRUE,
  infrequent_term_threshold = 0.2,
  verbose = T)
#this takes a little while: 8-10 mins

#let's get the results! (20 seconds or so)
preText_results <- preText(
  preprocessed_documents,
  dataset_name = "Some bills",
  distance_method = "cosine",
  num_comparisons = 20,
  verbose = FALSE)

#take a look at...
#preText_score_plot(preText_results)
# higher numbers on x axis => more "unusual" pairwise distances under this spec

#and
#regression_coefficient_plot(preText_results, remove_intercept = TRUE)
