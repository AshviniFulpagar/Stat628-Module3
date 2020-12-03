# Yelp Shiny App GUIDE

# GUIDE
This shiny app is based on the data from Yelp. We mainly select the restaurants which sell Chinese food. There are 3 main panels in our shiny.

**Yelp map**: 
It is an Interactive map. You can use "+", "-" or mouse to find the location on map. The marker is the real location of the restaurant.
When you click the marker, it will show the basic information of this restaurant and the other 2 Panels will show Reviews summary and suggestion
based on Yelp data.

**Customer Reviews** 
This part will need about one minute time to generate word cloud and barplots on each keyword. 
In this panel, we generate top 12 key words that shown more in the customer reviews and make a word cloud to make it easy to know. 
Then we make 12 plots to show the star given from the customer when he/she mention this key word in review. It will easily show what the customers focus on this restaurant and the level
of each key aspect.

**Business Suggestions**
We use Wilcoxon test to detect whether a key word/aspect is better, worse or near the average of all the restaurants with Chinese food. 
In order to understand easily, we use score=(left test's p-value)*100, thus to show the level of each key aspect and give suggestions based on the scores.


# README
**Writer:** Qingchuan Ji
**Other Partial Code Contributer:** Luyang Fang, Xiaotian Wang 
**Shiny App Examiner:** Qingchuan Ji, AshviniLuyang Fang, Xiaotian Wang, 

This shiny app is used for STAT 628 module3 Yelp Data Project. If you have any questions or suggestions, 
feel free to contact **Qingchuan Ji** at **qji5@wisc.edu**. It is our great honor to discuss with you.




