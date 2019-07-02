#7/12/2019
#Day 2 of EITM

rm(list=ls())
require(quanteda)
require(readtext)
require(stylest)

#lets grab ~24 House Bills as data
rt <- readtext("house_bills/*")
my.corpus <- corpus(rt)

#####################
# LEXICAL DIVERSITY #
#####################

#take a look at the type-token ratio for the manifestos
Types <- summary(my.corpus)$Types
Tokens <- summary(my.corpus)$Tokens
TTR <- Types/Tokens

#hmm, maybe this overstates things?
# take a look at Guiraud's Root measure
ntypes_G  <-  ntype(my.corpus) 
ntokens_G <-  ntoken(my.corpus) 

R <- ntypes_G/sqrt(ntokens_G)

###############
# READABILITY #
###############

#look at their readability using conventional measures
rf <- textstat_readability(my.corpus, c("Flesch","Dale.Chall.old"))

#weird things can happen --
sentance <- "These include capital expenditures by the Rural Electrification 
Administration and expenditures for resource development by other 
organizational units in the Department of Agriculture 
which are also mentioned above under 'agricultural programs.' "

textstat_readability(sentance, measure='Flesch')
#hmm...

#################
# BOOTSTRAPPING #
#################

#probably more efficient ways to do this, but here's one idea
# First, write a function that bootstraps at the sentance level within a doc
boot_doc <- function(document=my.corpus[1],nboot=50){
  #tokenize to the sent level
  toked_doc <- tokens(document, what='sentence')  
  #make it into a corpus
  sent_corpus<-corpus(unlist(toked_doc))
  
  #set up a vector to take mean of bootstrap stats
  means <- c()
  
  #sample the sentences (with replacement)
  #do this nboot times
  for(i in 1:nboot){
    samp_corpus <- sent_corpus[sample(1: nrow(sent_corpus$documents), replace=T)]
    #to get a sense of the problem, take a look at summary(samp_corpus)!
    
    #apply FRE to each of those, take mean
    tsr <-textstat_readability(samp_corpus, measure='Flesch' )
    FRE_mean <- mean(tsr$Flesch)
    means <- c(means, FRE_mean)
    mean_FRE <- mean(means)
    FRE_lower <- quantile(means, c(0.025))
    FRE_upper <- quantile(means, c(0.975))
    cat("done",i,"of",nboot,"resamples\n")
  }
  c(FRE_lower, mean_FRE, FRE_upper)
}

#so, for example, 
boot_h2 <- boot_doc(my.corpus[2], nboot=5) 

#############################
# look at Google books freq #
#############################
devtools::install_github("kbenoit/sophistication")


#####################
# M & W replication #
#####################

#from my colleague/coauthor Patrick Perry
# http://ptrckprry.com/course/ssd/lecture/federalist.html

####################
# Working with SHP #
# stylest          #
####################

#https://github.com/leslie-huang/stylest/blob/master/vignettes/stylest-vignette.md

#let's build a model

#first, make a decision about how to treat texts
filter <- corpus::text_filter(drop_punct = TRUE, drop_number = TRUE)
# let's use 80th percentile for terms
terms_80 <- stylest_terms(novels_excerpts$text, novels_excerpts$author, 80, filter = filter)
# make the model
mod <- stylest_fit(novels_excerpts$text, novels_excerpts$author, terms = terms_80, filter = filter)

# let's look at influential terms for this (very simple) model
influential_terms <- stylest_term_influence(mod, novels_excerpts$text, novels_excerpts$author)

#note that you can feed your own work in as a "new text" and then do 
#stylest_predict()

na_text <- "No one who had ever seen Catherine Morland in her infancy would have supposed 
            her born to be an heroine. Her situation in life, the character of her father 
            and mother, her own person and disposition, were all equally against her. Her 
            father was a clergyman, without being neglected, or poor, and a very respectable 
            man, though his name was Richard-and he had never been handsome. He had a 
            considerable independence besides two good livings-and he was not in the least 
            addicted to locking up his daughters."

pred <- stylest_predict(mod, na_text)


