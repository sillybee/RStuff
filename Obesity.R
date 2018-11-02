'Author:Sybil Dias ; Code Reference: R-Bloggers (https://www.r-bloggers.com/bump-chart/)'
'Used R version 3.4.1, run on R studio'
'Packages used: dplyr, magrittr, ggplot2'


getwd()
'setwd(---)'
ls()
db<-read.csv("Obesity in America.csv",TRUE,",")
newdb<-subset(db,Category=="Total")
dbstate<-subset(newdb,newdb$StateAbbr!="GU" & newdb$StateAbbr!="PR" & newdb$StateAbbr!="US"& newdb$StateAbbr!="DC" )
library(dplyr)#data manipulation
library(magrittr)#pipe operator
library(ggplot2)#visual chart
db.rankings <- dbstate %>%
  +     group_by(Year) %>%
  +     arrange(Year, X.Obese, StateAbbr) %>%
  +     mutate(my_ranks = order(order(Year,X.Obese, StateAbbr, decreasing=TRUE)))%>%
  +     as.data.frame()
db.rankings <- db.rankings %>%
  +     mutate(flag = ifelse(db.rankings$StateAbbr %in% 
                               c("LA","AL","MS","WV","KY","AR","KS","OK","TN","MO"), 1, 0),
               +            country_col = ifelse(flag == 1, db.rankings$StateAbbr, "zzz"))
db.rankings <- db.rankings %>%
  mutate(flag = ifelse(db.rankings$StateAbbr %in% c("LA","AL","MS","WV","KY","AR","KS","OK","TN","MO"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, db.rankings$StateAbbr, "zzz")
  )

db.rankings <- db.rankings %>%
  +     mutate(flag = ifelse(db.rankings$StateAbbr %in% c("LA","AL","MS","WV","KY","AR","KS","OK","TN","MO"), TRUE, FALSE),
               +            country_col = ifelse(flag == TRUE, db.rankings$StateAbbr, "zzz")
               +     )

ggplot(data = db.rankings, aes(x = Year, y = db.rankings$my_ranks, group = db.rankings$StateAbbr)) +
  +     geom_line(aes(color = db.rankings$country_col, alpha = 1), size = 2) +
  +     geom_point(aes(color = db.rankings$country_col, alpha = 1), size = 4) +
  +     scale_y_reverse(breaks = 1:nrow(db.rankings)) +
        labs(x="Years", y="Rank", title="States Ranked by Total Obesity Percentages")
  + scale_color_manual(values = c("red","black","green","orange","yellow","blue","brown","cyan","magenta","purple",rep("grey",each=40))) 
  +  theme(legend.position = "none") 
  + geom_text(label=db.rankings$StateAbbr)


ggplot(data = db.rankings, aes(x = Year, y = db.rankings$my_ranks, group = db.rankings$StateAbbr)) +
  geom_line(aes(color = db.rankings$country_col, alpha = 1), size = 2) +
  geom_point(aes(color = db.rankings$country_col, alpha = 1), size = 4) +
  scale_y_reverse(breaks = 1:nrow(db.rankings)) +labs(x="Years", y="Rank", title="States Ranked by Total Obesity Percentages")+scale_color_manual(values = c("red","black","green","orange","yellow","blue","brown","cyan","magenta","purple",rep("grey",each=40))) +  theme(legend.position = "none") +geom_text(aes(label=db.rankings$StateAbbr) )
