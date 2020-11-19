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


########## 1. clean business.json #########

nrow(business) # 36327 samples in total
missing = apply(business,2,function(x) sum(is.na(x)))

# grep("Chinese|Asian",business$categories,value=TRUE,ignore.case=TRUE)  # contents
ind = grep("Chinese|Asian",business$categories,ignore.case=TRUE) #index of samples we are interested in
business_new = business[ind,]
nrow(business_new) # 960 samples left

## transfer to data.frame
att = as.data.frame(business_new[[12]])
hr = as.data.frame(business_new[[14]])
business_clean = cbind(business_new[1:11],att,business_new[13],hr)
colnames(business_clean)[12:50] = paste("attribute.",colnames(business_clean)[12:50],sep="")
colnames(business_clean)[51:58] = paste("hours.",colnames(business_clean)[51:58],sep="")

## delete some useless variables
# column
missing_clean = apply(business_clean,2,function(x) sum(is.na(x)))
dele_col = c(which(colnames(business_clean) %in% c("postal_code","latitude","longitude")),
         which(missing_clean>650)) # number of missing value larger than 650
business_clean = business_clean[,-dele_col]
#row
missing_row = apply(business,1,function(x) sum(is.na(x[9:length(x)])))
dele_row = which(missing_row>20) # the first 8 varaibles have no NA, 
                                 # delete the samples with more than 20 NAs of the remaining 26 variables
business_clean = business_clean[-dele_row,]

## write clean .csv file "business_clean.csv"
write.csv(business_clean,"business_clean.csv",row.names=FALSE)


######### 2. clean review.json ##############
review_clean = review[which(review$business_id %in% business_clean$business_id),]
## write clean .csv file "review_clean.csv"
write.csv(review_clean,"review_clean.csv",row.names=FALSE)


######## 3. clean user.json ################
user_clean = user[which(user$user_id %in% review_clean$user_id),]
## write clean .csv file "user_clean.csv"
write.csv(user_clean,"user_clean.csv",row.names=FALSE)


########## 4. clean tip.json ###########
tip_clean = tip[which(tip$business_id %in% business_clean$business_id),]
## write clean .csv file "tip_clean.csv"
write.csv(user_clean,"tip_clean.csv",row.names=FALSE)


########### read the clean data #########
# rm(list=ls())
# business = read.csv("business_clean.csv",header=T)
# review = read.csv("review_clean.csv",header=T)
# tip = read.csv("tip_clean.csv",header=T)
# user = read.csv("user_clean.csv",header=T)

