library(tidytext)
library(dplyr)


# To get the distribution of stars for a word in review data
getDistr <- function(word, dataset){
  index = grep(word, dataset$text)
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


keyword = "food"

business_id = "7j0kor_fkeYhyEpXh4OpnQ"


# Read the data, you can also change these two lines to read .csv files
dataReview = readRDS(file = "reviewDATA")
dataBusiness = readRDS(file = "businessDATA")


Reviews  = dataReview[which(dataReview$business_id==business_id),]



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
          legend.text = c("This shop", "Average"),
          ylim = c(0,min(max(elements)*1.5,1))
  ) 

  # Do the Wilcoxon Rank Sum and Signed Rank Test
  pvalue <- wilcox.test(sample1,sample2)$p.value %>% 
    print  

  # The significance level
  threshold = 0.05 

  
  if(pvalue < threshold){
    print("We can give suggestion based on this key word")
  }else{
    print("This keyword is not significant enough.")
  }
  
}

