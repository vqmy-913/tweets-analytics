

####### QUESTION 11: SOCIAL NETWORK GRAPH #######

# get user data from tweets data set
users <- users_data(tweets)
head(users$screen_name)

# create the 1st edge list by column binding the above users
el1 = cbind(tweets$in_reply_to_screen_name,users$screen_name)
colnames(el1) = c("mentioner","mentioned")

# remove rows with NA so they are not included in the network
el1 = na.omit(el1) 
head(el1)

  # tweet 1 did not mention any user
  tweets$entities[[1]]$user_mentions$screen_name
  
  # tweet 2 mentioned 3 users
  tweets$entities[[2]]$user_mentions$screen_name

# collect the above mentions in a list called mentions
get_mentioned_screennames = function () {
  mentions = list()
  for(i in 1:length(tweets$entities)) {
    mentions[[i]] = tweets$entities[[i]]$user_mentions$screen_name
  }
  # mentions = mentions[!is.na(mentions)] 
  # remove NA values (i.e. tweets with no mentions) from the vector
  # mentions = unique(mentions) # return unique mentions only
  return(mentions)
}

mention_list = get_mentioned_screennames()
head(mention_list,3)

# create the 2nd edge list
a = mention_list
b = users$screen_name
d = c()

# repeat the tweeter's name for the number of people who they are mentioning
# place this in vector d
for (i in 1:length(a)){
  f = rep(b[i], length(a[[i]]))
  d = append(d, f)
}

# unwind the mention list and bind it with d to create the 2nd edge list
k = c(unlist(a))
el2 = cbind(d, k)

# remove NA which does not contribute to the final edge list
el2 = na.omit(el2)
head(el2)

# combine edge lists 1 and 2 to construct network graph
g = graph.edgelist(el)

# plot(g,layout=layout.fruchterman.reingold,vertex.size = 5, edge.arrow.size=0.1, vertex.label.cex=0.5)
# this graph is too large and slow in computing

# this looks better
g4 = induced_subgraph(g, which(degree(g, mode = "all") > 4))
plot(g4, layout = layout.random, vertex.size = 7, 
     edge.arrow.size=0.15, vertex.color="red",vertex.label.cex=0.8)

# the top 8 users who mentions the Twitter handle
centralIDs = order(degree(g),decreasing = TRUE)[1:8]
V(g)[centralIDs]
degree(g)[centralIDs]


####### QUESTION 13: TIME SERIES ANALYSIS #######

# retrieve 1,600 raw tweets and save object
tweetHandle = ""
tweets = get_timeline(tweetHandle, n = 1600, token = twitter_token,
                      include_rts = FALSE)

# save(tweets, file="comp3020.dataset_timeline.RData")
load("comp3020.dataset_timeline.RData")

# in the above timeline, we extract the latest 550 posts
tweets550 = tweets[1:550,]
head(tweets550$created_at)

# obtain dates from the 1st column when the tweets are made
dates = sort(format(tweets550$created_at,'%Y-%m-%d'), decreasing=FALSE)
head(dates)
length(unique(dates))

# c is a temporary data frame that summarises all the tweet counts by dates
# note that the 550 collected tweets are unevenly spread across the collected time frame
# so c would contain tweet counts for unique dates only
c = as.data.frame(dates) %>% group_by(dates) %>% summarise(counts = n())
c$dates <- as.Date(c$dates)
head(c) # is a 319-row data frame

# s is a temporary empty data frame that contains every 
# single date from 2021-02-18 and 2022-09-30
x.axis = c(seq(as.Date("2021-02-18"), as.Date("2022-09-30"), by="days"))
s = cbind.data.frame(
  dates = x.axis,
  counts = rep(0,length(x.axis))
)
head(s) # is a 590-row data frame

# do a full-join between s and c to obtain the plotting data set, df
# note that previously, c$dates has been transformed into the corresponding
# format to s$dates so merge() should work without errors
df <- merge(s, c, all = TRUE, by = "dates")

# drop unnecessary 2nd column and replace NA's in the 3rd column with 0's
df <- df[,-2]
df[is.na(df)] <- 0

# rename accordingly
colnames(df) = c("dates","counts")

# create a new column "day", for plotting
df <- df %>% mutate(day = row_number())
head(df)

### Visualisation ###
# time-series plot of tweet counts
plot(df$day, df$counts,
     xlab = "Day", ylab = "Tweet counts")

# to calculate the trend line and seasonal line in the time series
windowWeights = function(m) {
  if( m%%2 ) {
    ## m is odd
    w = rep(1,m)/m
  } else {
    ## m is even
    ## compute w for an even sized window
    w = c(0.5,rep(1,m-1),0.5)/m
  }
  return(w)
}

moving.average = function(x, m) {
  ## compute window weights
  w = windowWeights(m)
  j = floor(m/2)
  offsets = (-j):j
  n = length(x)
  res = rep(NA, n)
  ## slide window and apply window weights to obtain averages
  for(i in (j+1):(n-j)) {
    res[i] = sum(w*x[i+offsets])
  }
  return(res)
}

Y = sqrt(df$counts)
trend = moving.average(Y, 30)

centred = sqrt(df$counts) - trend
zcentred = centred[!is.na(centred)]
seasonal = function(x ,m) {
  ## extend the data so it is a multiple of m long
  tmp = c(rep(NA, m - length(x)%%m),x)
  ## convert to a matrix
  mat = matrix(tmp, nrow=m)
  ## Calculate the row means to get the seasonal component (excluding missing entries)
  seas = rowMeans(mat, na.rm=TRUE)
  seas = seas - mean(seas)
  return(seas)
}

season = seasonal(zcentred,30)

### Final plot with time-series data, with the trend and seasonal lines
plot(df$day, Y)
lines(df$day,trend,col=2)
lines(df$day[!is.na(centred)],trend[!is.na(centred)] +
        rep_len(season,length.out=sum(!is.na(centred))),col=5)


# do a hypothesis test to see if there is a linear relationship between the dates and the number of posts.
# H0: no linear relationship between the date and the number of posts
# H1: there is a linear relationship between the date and the number of posts
m = lm(df$counts~df$day)
summary(m) # p < 0.05
par(mfrow=c(2,2))
plot(m) # based on this plot, the data is not normally distributed

# create another lm with transformed y (with square root)
m1 = lm(sqrt(df$counts)~df$day)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
