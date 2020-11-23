rm(list=ls())
# setwd("C:/Users/86159/Desktop/Fall2020/628/module3/code")

# install.packages("rjson")
# install.packages("jsonlite")
library(rjson)
library(jsonlite)

business = jsonlite::stream_in(file("../data/business_city.json"))
user = jsonlite::stream_in(file("../data/user_city.json"))
tip = jsonlite::stream_in(file("../data/tip_city.json"))
review = jsonlite::stream_in(file("../data/review_city.json"))

# tranfer "business" into data.frame
business_new = business
att = as.data.frame(business_new[[12]])
hr = as.data.frame(business_new[[14]])
business_clean = cbind(business_new[1:11],att,business_new[13],hr)
colnames(business_clean)[12:50] = paste("attribute.",colnames(business_clean)[12:50],sep="")
colnames(business_clean)[52:58] = paste("hours.",colnames(business_clean)[52:58],sep="")


######## function #############
## Usage: data_grep(name); used to extract business with specific category "name"
## Input: name
## Example: data_grep("Chinese")
## Note: should create a directory called "clean_data" at first

data_grep = function(name){
  ## 1. clean business.json
  ind = grep(name,business_clean$categories,ignore.case=TRUE)
  business_grep = business_clean[ind,]
  write.csv(business_grep,paste("./clean_data/business_",name,".csv",sep=''),row.names=FALSE)
  ## 2. clean review.json
  review_grep = review[which(review$business_id %in% business_grep$business_id),]
  write.csv(review_grep,paste("./clean_data/review_",name,".csv",sep=''),row.names=FALSE)
  ## 3. clean user.json
  user_grep = user[which(user$user_id %in% review_grep$user_id),]
  write.csv(user_grep,paste("./clean_data/user_",name,".csv",sep=''),row.names=FALSE)
  ## 4. clean tip.json
  tip_clean = tip[which(tip$business_id %in% business_grep$business_id),]
  write.csv(tip_clean,paste("./clean_data/tip_",name,".csv",sep=''),row.names=FALSE)
}


data_grep("Chinese")
data_grep("wine")
data_grep("breakfast")
