## ------------
rm(list = ls())
library(tidytext)
library(dplyr)
library(Matrix)
library(pluralize)
library(wordcloud)
#Our working directory in this part is "Stat628-Module3/code", if the directory is not the same,
#please use setwd() function to change the directory.
## ----------------------------------
# To get the distribution of stars for a word in review data
getDistr <- function(word, dataset){
  dataset$text = tolower(dataset$text)
  word = paste(word,"|",singularize(word),"|",pluralize(word),sep="")
  index = grep(word, dataset$text,ignore.case = TRUE)
  if(length(index)==0)
  {
    print("word not detected.")
    return(1)
  }
  stars = dataset$stars[index]
  elements = rep(0,5)
  for(i in 1:5)
  {
    elements[i] = sum(stars==i)/length(stars)
  }
  # barplot(elements,names.arg = c("1","2","3","4","5"))
  return(elements*length(stars))
}


# To get the most frequently used words in review data
mostFreq <- function(dataset, maxrownum)
{
  text_df <- tibble(index = 1:nrow(dataset), review = dataset$text)
  tt  = text_df %>% unnest_tokens(word, review)
  A = cast_sparse(tt, index, word)
  wordlist = colSums(A)
  wordlist = sort(wordlist,decreasing = TRUE)
  wordlist = as.data.frame(wordlist)
  wordlist$word = rownames(wordlist)
  colnames(wordlist) <- c("count","word")
  cleaned_wordlist <- wordlist %>%
    anti_join(get_stopwords())
  return(cleaned_wordlist[1:maxrownum,])
}

## -------------------------------------

business_id = "iKzXIGPzUAuM2hs2RXEQFw"

wordlist = read.csv(file ="../wordList/wordList_V2.csv")

dataReview = read.csv("../clean_data/Chinese/review_Chinese_new.csv")
dataBusiness = read.csv("../clean_data/Chinese/business_filter.csv")

## ---------------------------------


Reviews  = dataReview[which(dataReview$business_id==business_id),]

#In this turn, we choose top 16 key words to make 4*4=16 barplots, if you like, you can increase/decrease the key
#words count.(But it may need to change par() function below.)
frequentword = mostFreq(Reviews,100)
word = c()
i=1
count=0
while(i<100){
  
  if(frequentword$word[i] %in% wordlist$word)
  {
    word = rbind(word, frequentword[i,])
    count = count+1
  }
    if(count==18){
      break}
  i=i+1
}


# Read the data, you can also change these two lines to read .csv files
# dataReview = readRDS(file = "reviewDATA")
# dataBusiness = readRDS(file = "businessDATA")

#Draw 4*4=16 bar plots and do Wilcoxon test on each keywords
#set empty vectors for p-values

pvalue<-c()
pvalue1<-c()
pvalue2<-c()

p<-par(mfrow=c(3,3),no.readonly = TRUE)
for(i in 1:18)
{keyword<-word$word[i]
freq1 = getDistr(keyword,Reviews)
sample1 = rep(1:5, freq1)
freq2 = getDistr(keyword, dataReview)
sample2 = rep(1:5,freq2)

if(sum(freq1)==0){
  print("This keyword is not detected in the reviews")
}else{
  
  # Elements for barplot
  elements = rep(0,10)
  elements[c(1,3,5,7,9)]=freq1/sum(freq1)
  elements[c(2,4,6,8,10)]=freq2/sum(freq2)

barplot(elements,
names.arg = c("1","1","2","2","3","3","4","4","5","5"),
col = c(1,2,1,2,1,2,1,2),
xlab = "stars",
ylab = "frequency",
space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
main=keyword,
ylim = c(0,min(max(elements)*1.5,1))) 
legend("topleft",c("This shop", "Average"),cex=0.8,fill=c("black","red"))
  # Do the Wilcoxon Rank Sum and Signed Rank Test
  pvalue1[i] = wilcox.test(sample1,sample2,alternative = "less")$p.value
  pvalue2[i] = wilcox.test(sample1,sample2,alternative = "greater")$p.value
  pvalue [i]= wilcox.test(sample1,sample2)$p.value



}
}

#Getting the word cloud of the restaurant
p<-par(mfrow=c(1,1),no.readonly = TRUE)
wordcloud(word$word,word$count)


#Based on the Wilcoxon test, we can get the suggestions:


paste0("Based on ",dataBusiness$review_count[dataBusiness$business_id==business_id]," reviews on Yelp, we will produce these suggestions:")
for(i in 1:length(word$word)){
  keyword<-word$word[i]
  if (pvalue1[i]>=0.95){print(paste0("The ",keyword," is better than the average of other restaurants. Please keep this advantage"))}
  else if(pvalue1[i]<=0.05){print(paste0(keyword," seems worse than the average of other restaurants. Please do some improving on this aspect"))}
  
}

#Another type of giving suggestions
advantage = c()
disadvantage = c()
for (i in 1:length(word$word)){
  if (pvalue1[i]>=0.95){
    advantage = c(advantage,word$word[i])
  }else if (pvalue1[i]<=0.05){
    disadvantage = c(disadvantage,word$word[i])
  }}

cat("You have good performance in the following aspects:",'\n',
    paste(advantage,collapse=', '),'\n')
cat("Your performance seems worse than the average level of other restaurants in the following aspects:",'\n',
    paste(disadvantage,collapse=', '),'\n')



