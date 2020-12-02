# Stat628-Module3

**Yelp data analysis**

- all scripts of the projects can be found at [code](https://github.com/AshviniFulpagar/Stat628-Module3/tree/main/code)
- clean data is stored at
[clean_data](https://github.com/AshviniFulpagar/Stat628-Module3/tree/main/clean_data)

Updata 2020/12/1:

Please use the data in "data_V2.zip" for the analysis after data pre-processing part.

Updata 2020/11:

The directory "clean_data" stores the data we grep from the whole dataset used for our analysis.


**Process:**

*Data pre-processing part:*

1. Based on "function_grep.R" to get the clean_data "review_Chinese.csv", "business_Chinese.csv". (grep business and corresponding reviews with a specific category you are interested in)

2. Based on "wordList.RMD" to get the new reviews file "review_Chinese_new.csv"

3. Based on "wordList.RMD" to realize NLP and get "wordList_V2.csv"(contains the list of words which will be used in the following analysis)

4. Based on "wordList.RMD" to get the new "business_filter.csv" file. (filter some businesses with little reviews)


*Further analysis:*

