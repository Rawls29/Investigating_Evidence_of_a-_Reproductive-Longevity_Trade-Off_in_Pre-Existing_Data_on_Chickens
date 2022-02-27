#Clearing envionment
rm(list=ls())
#Setting working directory
setwd('C:/Users/samra/Documents/My Documents/Uni/Imperial/Winter Project/R')

#Loading useful libraries
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)

#Importing litsearchr from GitHub
library(devtools)
install_github("elizagrames/litsearchr", ref="main")

#Loading litsearchr
library(litsearchr)

#Importing results of naive search in CAB Direct
#('chicken' OR 'Gallus gallus' OR 'hen) AND lifespan AND (breed OR strain OR line)
naive_results_WoS <- import_results(file="chicken_lifespan.RIS")
nrow(naive_results_WoS) #Number of rows= number of results
colnames(naive_results_WoS)

#Getting potential search terms#

#Extracting keywords
keywords <- extract_terms(keywords=naive_results_WoS[,"keywords"], method="tagged", 
                          min_n=1, min_freq=3)

#Analysing Titles and Abstracts
title_terms <- extract_terms(text=naive_results_WoS[, "title"], method="fakerake", 
                             min_freq=2, min_n=1)
#Can use stopwords= to define a list of words which shouldn't be included
#(i.e. general scientific words)

#Using network analysis to find out which terms co-occur together to see which
#likely refer to the same topic
#Analysing co-occurance of terms in abstracts

#Creating a document containing the article titles and abstracts
docs <- paste(naive_results_WoS[, "title"], naive_results_WoS[, "abstract"])

#Creating a matrix of which terms appear in which article
#dfm stands for "document-feature matrix"
dfm <- create_dfm(elements=docs, features=title_terms)

#Using create_network() to turn the dfm into a network
#min_studies= defines how many articles a word needs to be in to be included
g <- create_network(dfm, min_studies=3)

#Visualising the network
ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)
#Uses 'Kamada and Kawai' layout, which draws terms that are more closely linked
#together close together
#geom_node_text() labels the points
#check_overlap=TRUE only retains links which don't overlap
#geom_edge_link() adds lines linking the terms

#Pruning the network
#The 'strength' of each term in the network is the number of other terms it 
#co-occurs with
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths
#Most weakly linked terms are at top of list

#Plotting strengths
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>1), hjust="right", nudge_y=2, 
            check_overlap=TRUE)

cutoff_fig

#Cumulative pruning
#Choosing a certain proportion of selection terms (e.g. 80%)
cutoff_cum <- find_cutoff(g, method="cumulative", percent=0.8)

cutoff_cum

#Plotting cut off onto graph
cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")

#Getting keywords from graph
get_keywords(reduce_graph(g, cutoff_cum))

selected_terms_cum <- get_keywords(reduce_graph(g, cutoff_cum))

#Changepoint pruning
#Placing 3 cutoffs
cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)

cutoff_change

cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

g_redux <- reduce_graph(g, cutoff_change[3])
selected_terms_cutoff <- get_keywords(g_redux)

selected_terms_cutoff
