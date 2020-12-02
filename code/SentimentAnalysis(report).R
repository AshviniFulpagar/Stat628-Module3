## ------------
rm(list = ls())

library(tidytext)
library(dplyr)
library(Matrix)
library(pluralize)
library(wordcloud)
## ----------------------------------
# To get the distribution of stars for a word in review data
getDistr <- function(word, dataset){
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
  return(wordlist[1:maxrownum,])
}

## -------------------------------------

business_id = "HVpwpXneaCWMeEBF7H8jpQ"
wordlist = read.csv(file ="../clean_data/data_V2/wordList_V2.csv")

dataReview = read.csv("../clean_data/data_V2/review_Chinese_new.csv")
dataBusiness = read.csv("../clean_data/data_V2/business_filter.csv")

## ---------------------------------


Reviews  = dataReview[which(dataReview$business_id==business_id),]

frequentword = mostFreq(Reviews,400)
word = c()
i=1
count=0
while(i<400){
  
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


for(keyword in word$word)
{
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
          names.arg = c("1","","2","","3","","4","","5",""),
          col = c(1,2,1,2,1,2,1,2),
          xlab = "stars",
          ylab = "frequency",
          space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
          legend.text = c("This shop", "Average"),
          ylim = c(0,min(max(elements)*1.5,1)),
          main = keyword
  ) 

  # Do the Wilcoxon Rank Sum and Signed Rank Test
  pvalue1 = wilcox.test(sample1,sample2,alternative = "less")$p.value

#    giveSuggestion(keyword, pvalue1, pvalue2)
    cat(keyword,pvalue1,score=round(pvalue1*100,1),"\n")
}
}
wordcloud(word$word,word$count)
