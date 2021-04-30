#Project Market Basket Analysis on Online Retail Data retrieved from UCL
#Created on : 09:02:2018
#Created by :Jing Zhang, Bolin Xie, Kanika Sharma


#Loading all the reuquired packages
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

#DATA COLLECTION 
#reading data from csv 
retail <- read_excel('D:/Desktop/DirectMarketing/Online Retail.xlsx')


#DATA CLEANSING 
#removing Missing values
retail <- retail[complete.cases(retail), ]
#filtering the country to United Kingdom
# retail <- subset(retail, Country %in% c("France"))
retail <- subset(retail, StockCode  != "POST")
#converting columns into factor datatype
retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))
#spliting InvoiceDate into Date and Time
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$Time <- as.factor(retail$Time)
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)
summary(retail)
head(retail)
# DATA VISUALIZATION
#Plot the time when the most customer are online
as.data.frame(retail)
a <-  ggplot(retail,aes(x=Time))
a +  geom_histogram(stat="count",fill="indianred")
#Plot the number of products generally bought by customers
detach("package:plyr", unload=TRUE)
retail %>%
  group_by(InvoiceNo) %>%
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) +
  geom_rug()+
  coord_cartesian(xlim=c(0,20),ylim=c(0,3))
# Plot the top sold products
tmp %>%
  group_by(StockCode, Description) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
tmp  <-  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()


detach("package:dplyr", unload=TRUE)
#ASSOCIATION RULES
#First step is to sort the data 
library(plyr)
retail_sorted <- retail[order(retail$CustomerID),]
#splitting data frame into seperate sections
itemList <- ddply(retail,c("CustomerID","Date"),function(df1)paste(df1$Description,collapse = ","))
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")
summary(itemList)
#writing the data frame into csv and reading from same to male thel transactions
write.csv(itemList,"market_baskett.csv", quote = FALSE, row.names = TRUE)
write.table(itemList, "mydata.txt", sep=" ",row.names = FALSE, col.names = FALSE, quote = FALSE)
tr <- read.transactions('market_baskett.csv', format = 'basket', sep=',')
# tr
dev.off()
LIST(tr)
inspect(tr)
#filtering transaction with size =3
filter_trains = tr[size(tr) >=10]
image(tr)
itemFrequencyPlot(tr)
summary(tr)
str(tr)
#removing quotes
#plotting the graph
itemFrequencyPlot(tr, topN=20, type='absolute')
itemFrequencyPlot(tr,
                  type="relative", # can be changed to absolute type 
                  topN=10, # can be changed to the number of interest
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, relative')
#least bouight items
barplot(sort(table(unlist(LIST(tr))))[1:10]/9835,
        horiz=TRUE,
        las=1,
        col='steelblue3',
        xlab='',
        main='Frequency, absolute')

#contingebcg table
tbl <- crossTable(tr)
tbl[1:5,1:5]
#applying aprori for support 0.01% and confidence 80%
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
# rules <- apriori(tr)
#lets play around
rules.sorted <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
#removing redundancy
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules.sorted[!redundant]
rules.pruned
#or try this
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  #> 3913
rules <- rules[-subsetRules] # remove subset rules. 
summary(rules)
#So we found no redundant rules,hence lets continue with rules.sorted
summary(rules)
inspect(rules)
#converting the rules to data frame and viewing them
df_basket <- as(rules,"data.frame")
View(df_basket)
#see top 10 rules
inspect(rules[1:10])
#finding what led to purchase of product X
rules <- apriori (data=tr, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="DECORATION"), control = list (verbose=F)) # get rules that lead to buying 
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))
#pick the top ten rules
topRules <- rules[1:10]
plot(rules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")
plot(topRules, method="graph", control=list(type="items"))

# plot(rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(topRules, method ="graph",interactive=TRUE)
plot(topRules,measure=c("support","lift"),shading="confidence",interactive=T)


