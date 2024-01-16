# Group Project Script ----------------------------------------------------

# Preliminary Tasks -------------------------------------------------------

# import required packages
library("rtweet") # access Twitter API in R
library("tm") # used to convert tweets to a term frequency matrix
library("SnowballC") # used to stem the tweet text
library("wordcloud") # word cloud library for visualisation
library("dplyr") # Need for distinct function

# prepare data, by loading in the sourced data file
load("comp3020.dataset.RData") # use the set of tweets obtained

# Statistical Analysis ----------------------------------------------------

# Question 1 --------------------------------------------------------------

users <- users_data(tweets) # gets user ID so you can extract there follower and favorite count.

# favorite count keep consistent throughout analysis: low favourite
  # the followers count goes through each respective grouping
l_l=users[users$followers_count < 1000 & users$favourites_count < 300, ] # Extracts requested data including all variables.
l_l=distinct(l_l, l_l$id, .keep_all = TRUE) # Get unique data of every user based on user id. Removes duplicates
a_l=users[users$followers_count >= 1000 & users$followers_count < 5000 & users$favourites_count < 300, ]
a_l=distinct(a_l, a_l$id, .keep_all = TRUE)
h_l=users[users$followers_count >= 5000 & users$favourites_count < 300, ]
h_l=distinct(h_l, h_l$id, .keep_all = TRUE)

# favorite count keep consistent throughout analysis: average favourite
  # the followers count goes throughe each respective grouping
l_a=users[users$followers_count < 1000 & users$favourites_count >= 300 & users$favourites_count < 1000, ]
l_a=distinct(l_a, l_a$id, .keep_all = TRUE)
a_a=users[users$followers_count >= 1000 & users$followers_count < 5000 & users$favourites_count >= 300 & users$favourites_count < 1000, ]
a_a=distinct(a_a, a_a$id, .keep_all = TRUE)
h_a=users[users$followers_count >= 5000 & users$favourites_count >= 300 & users$favourites_count < 1000, ]
h_a=distinct(h_a, h_a$id, .keep_all = TRUE)

# favorite count keep consistent throughout analysis: high favourite
  # the followers count goes throughe each respective grouping
l_h=users[users$followers_count < 1000 & users$favourites_count >= 1000, ]
l_h=distinct(l_h, l_h$id, .keep_all = TRUE)
a_h=users[users$followers_count >= 1000 & users$followers_count < 5000 & users$favourites_count >= 1000, ]
a_h=distinct(a_h, a_h$id, .keep_all = TRUE)
h_h=users[users$followers_count >= 5000 & users$favourites_count >= 1000, ]
h_h=distinct(h_h, h_h$id, .keep_all = TRUE)

# create data frame with columns being favorite count and use nrow() to find count
count = data.frame(
  "Low" = c(nrow(l_l), nrow(a_l), nrow(h_l)), 
  "Average" = c(nrow(l_a), nrow(a_a), nrow(h_a)), 
  "High" = c(nrow(l_h), nrow(a_h), nrow(h_h)))

rownames(count) = c("Low", "Average", "High") # add rownames to columns based on follower count
print(count)

conTable <- as.matrix(count) # convert data frame to a matrix
names(dimnames(conTable)) <- c("Favourites Count", "Followers Count") # name the dimnames
print(conTable)

# Question 2 --------------------------------------------------------------

# H_0 = the favourite count is not related (independent) to the follower count
# H_1 = the favourite count is related (dependent) to the follower count

chisq.test(conTable)
chisq.test(conTable, simulate.p.value = TRUE)

# In either case, when simulating the p-value and also when performing the
  # chi-squared test as per normal conditions, the p-value is extremely
  # small, so we can succcessfully reject the null hypothesis.

# This means we found evidence that the alternative hypothesis is true, 
  # and conclude the followers' count and favourites count are related

# Text Mining -------------------------------------------------------------

# Question 3 --------------------------------------------------------------

# create a corpus from tweet text - this will convert all the tweets into 
  # a vector containing each tweet with each new element being a new tweet
tweet.corpus = Corpus(VectorSource(tweets$text))

# convert the characters to ASCII
corpus = tm_map(tweet.corpus, function(x) iconv(x, to='ASCII', sub=' '))

corpus = tm_map(corpus, removeNumbers) # remove numbers
corpus = tm_map(corpus, removePunctuation) # remove punctuation
corpus = tm_map(corpus, stripWhitespace) # remove whitespace
corpus = tm_map(corpus, tolower) # convert all to lowercase
corpus = tm_map(corpus, removeWords, stopwords()) # remove stopwords
corpus = tm_map(corpus, stemDocument) # convert all words to their stems

# to perform clustering, obtain document vectors contained in the 
  # document term matrix, and extract the matrix and apply TF-IDF weighting

tweet.tdm = TermDocumentMatrix(corpus) # create a term document matrix
tweet.wtdm = weightTfIdf(tweet.tdm) # TDM using TF-IDF weights (using R function)
tweet.matrix = t(as.matrix(tweet.wtdm)) # change TDM to a document term matrix

# remove empty tweets so they don't effect the calculations
empties = which(rowSums(abs(tweet.matrix)) == 0)
empties # returns integer(0) as there are no empty tweets
length(empties) # return length of empties vector

# create a conditional statement, where if length of empties is 0 return 
  # the tweet.matrix as it is, otherwise remove empty tweets in matrix
if (length(empties) == 0) {
  tweet.matrix = tweet.matrix
} else {
  tweet.matrix = tweet.matrix[-empties, ]
}

dim(tweet.matrix) # view the dimensions of the document term matrix (DTM)
head(tweet.matrix) # view the first few rows of the DTM

# Question 4 --------------------------------------------------------------

# sum tweets term frequencies (remember in DTM, terms are columns)
freqs = colSums(tweet.matrix)
freqs

# remove any words that have count "NA" (named vector)
  # as these values are not useful for calculation
freqs = freqs[!is.na(freqs)]
freqs

names(freqs) # showing names from frequencies

# build the word cloud (needs term names and frequencies)
wordcloud(names(freqs), freqs, random.order = FALSE)

# modifying the output - with minimum frequency of 3
wordcloud(names(freqs), freqs, random.order = FALSE, min.freq = 3,
          colors = brewer.pal(8, "Dark2"))

# Question 5 --------------------------------------------------------------

# matrix is already in document term (DTM) form, so no need to transpose

dim(tweet.matrix)[1] # check how many rows/documents we have, to set max bound

# perform MDS using 1000 dimensions (best bet for dimension choice)
  # note, only 1051 of the first 1400 eigenvalues are > 0

maxD = dist(tweet.matrix, method = "maximum") # maximum distance
mdsMaximum <- cmdscale(maxD, k=1000)

plot(mdsMaximum[, 1], mdsMaximum[, 2], pch = 16,
     main = "MDS: TF-IDF Weighted Maximum Distance")

manhatD = dist(tweet.matrix, method = "manhattan") # manhattan distance
mdsManhattan <- cmdscale(manhatD, k=1000)

plot(mdsManhattan[, 1], mdsManhattan[, 2], pch = 16,
     main = "MDS: TF-IDF Weighted Manhattan Distance")

# Question 6 --------------------------------------------------------------

# clustering for maximum distance matrix

# use the elbow method to identify an appropriate number of clusters, 
  # then compute the clusters and store them in K

n = 15  # starting with 15 clusters
maximumSSW = rep(0, n) # prepare SSW vectors (values to record)

for (a in 1:n) {
  # nstart is the number of random sets in each cluster
  K = kmeans(mdsMaximum, a, nstart = 20)
  maximumSSW[a] = K$tot.withinss # total within cluster sum of squares
}

plot(1:15, maximumSSW, type="b") # view the elbow plot

# we can see an elbow at 6 clusters ??

                      ##############################

# clustering for manhattan distance matrix

# use the elbow method to identify an appropriate number of clusters, 
  # then compute the clusters and store them in K

n = 15  # starting with 15 clusters
manhattanSSW = rep(0, n) # prepare SSW vectors (values to record)

for (a in 1:n) {
  # nstart is the number of random sets in each cluster
  K = kmeans(mdsManhattan, a, nstart = 20)
  manhattanSSW[a] = K$tot.withinss # total within cluster sum of squares
}

plot(1:15, manhattanSSW, type="b") # view the elbow plot

# We can see an elbow at 8 clusters ??
  # then a further drop indicating that there may be clusters in clusters 
  # (a hierarchy of clusters), so we will compute k-means using 8 clusters

# Question 7 --------------------------------------------------------------

# k-means clustering for maximum distance matrix
maxK = kmeans(mdsMaximum, 6, nstart = 20) # compute using 6 clusters
summary(maxK)

table(maxK$cluster) 

# k-means clustering for manhattan distance matrix
manhatK = kmeans(mdsManhattan, 8, nstart = 20) # compute using 8 clusters
summary(manhatK)

table(manhatK$cluster)

# It is to note though that for the different distance matrix clusterings', both have the same SSW and SSB value, 
  # where the best number of clusters for each matrix is provided where the reduction of SSW slows (the elbow bend)

# Question 8 --------------------------------------------------------------

# k-means clustering visualisation for maximum distance matrix

mdsMaximum_2D <- cmdscale(maxD, k=2) # perform MDS using 2 dimensions
plot(mdsMaximum_2D, col = maxK$cluster) # plot and colour points using cluster number

# k-means clustering visualisation for manhattan distance matrix

mdsManhattan_2D <- cmdscale(manhatD, k=2) # perform MDS using 2 dimensions
plot(mdsManhattan_2D, col = manhatK$cluster) # plot and colour points using cluster number

# Question 9 --------------------------------------------------------------

# The best distance matrix for analysis of the data in regards to the 2D visualisation is the manhattan distance.
# This is due to very spread out data in comparison to the maximum distance in which they cover very limited points.

# When looking at the k-means clusters for each distance matrix, it is evident that manhattan distance has
  # a better distribution of values, hence attributing to the more accurate spread of data points.

# However, in respect to the elbow distance the maximum distance has a better output as the elbow is much easier to 
  # visualise in comparison to the manhattan distance where it rather hard to define the elbow.

# Question 10 -------------------------------------------------------------

# create a dendrogram for each cluster by performing hierarchical clustering

# select words that appear most often and examine how they are clustered
  # choose set of words that appear in at least 100 tweets and store their index. THis increase the visiblility of the denogram!!!!!!!!!!!!!!!!!!!!!
frequent.words = which(apply(tweet.matrix > 0, 2, sum) > 100)

# extract only those columns from the tweet matrix
term.matrix = tweet.matrix[,frequent.words]

dim(term.matrix) # check new term document matrix size

# compute the distance matrix containing the manhattan distance between 
  # all chosen terms, then compute and plot the hierarchical clustering

D = dist(term.matrix, method = "manhattan")

singleD = hclust(D, method = "single") # single linkage clustering
plot(singleD, main = "Single Linkage Clustering Dendogram")

completeD = hclust(D, method = "complete") # complete linkage clustering
plot(completeD, main = "Complete Linkage Clustering Dendogram")

                    ##############################

# generate a word cloud of the words in each cluster
table(manhatK$cluster)
n = 8

# loop through each cluster, and create a word cloud for each
for (i in 1:n) {
  cluster.number = i # nominate each cluster, and explore it
  
  # find position of tweets in the manhattan clustering
  clusterTweetsId = which(manhatK$cluster == cluster.number)
  
  # extract tweets vectors for cluster
  clusterTweets = tweet.matrix[clusterTweetsId,]
  
  # combine the tweets into a mean tweet
  clusterTermWeight = colMeans(clusterTweets)
  
  # generate a word cloud to visualise the words in a cluster
  wordcloud(
    words = names(clusterTermWeight),
    freq = clusterTermWeight,
    min.freq = 3,
    max.words = 100,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}
                
# From the word cloud above, that words 'for', 'your', and 'thank' are the ones 
# standing out as indicated by their large size. 
# By taking the minimum frequency value of 3, we can see that words such as 'conversation', 'leadership',
# 'global' and 'data' are colour coded, which highlights Bill Gates' personality and traits 
# when it comes to his striving ambitions for his company.

# Building Networks -------------------------------------------------------

# Question 11 -------------------------------------------------------------


# Question 12 -------------------------------------------------------------


# Time Series -------------------------------------------------------------

# Question 13 -------------------------------------------------------------

