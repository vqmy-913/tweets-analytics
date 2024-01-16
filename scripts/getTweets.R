# Twitter API, August 2022
# record your authentication keys to create token as an environment variable
app = "" 
key = ""
secret = ""
access_token = ""
access_secret = ""

# perform OAuth authentication
twitter_token = create_token(app, key, secret, access_token, access_secret,
                             set_renv= FALSE)

# retrieve 1,600 raw tweets and save object
tweetHandle = ""
tweets = search_tweets('Bill Gates', n = 1600, type = "recent", 
                       token = twitter_token, include_rts= TRUE, lang='en')

save(tweets, file="comp3020.dataset.RData")
# save(tweets, file="comp3020.dataset.RData")

# prepare data, by loading in the sourced data file
load("comp3020.dataset.RData") # use the set of tweets obtained
class(tweets) # quick check of loaded data
names(tweets) # examine the column headings
