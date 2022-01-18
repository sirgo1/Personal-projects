#Loading in the dataset.
#This code assumes that the dataset is in your working directory
ID86 <- read.csv("data00.csv")

View(ID86)

#Question 1a

#Checking for all unique days of the week

unique(ID86$Day)

#Renaming Tuesday and Friday

ID86$Day[ID86$Day=="Tuesday"] <-"Tues"
ID86$Day[ID86$Day=="Friday"] <-"Fri"


#Sub-setting for Tuesday evening in the whole of April and stored in Q1a

April <- ID86[grepl("Apr", ID86$Date),]
Tuesday <- subset(April, April$Day=="Tues")
Q1_a <- subset(Tuesday,Tuesday$Daytime=="night")

Q1_a <-sum(Q1_a$ItemsOrdered)         #The number of products purchased on Tuesday evenings throughout April


#Question 1b
Q1_b <- nrow(Q1_a)
Q1_b              #The number of purchase events on Tuesday evening throughout April


#Question 2a
One_product <- subset(ID86, ID86$ItemsOrdered=="1")
Q2_a <-nrow(One_product)
Q2_a          #the number of times 1 product was bought


#Question 2b
Q2_b <-max(ID86$ItemsOrdered)
Q2_b            #The highest amount of bought products in 1 single order

#Question 2c
plot(ID86$ItemsOrdered, main = "Items Ordered")
hist(ID86$ItemsOrdered, main = "Items Ordered")

#Question 3a
men <- subset(ID86, ID86$Gender=="Male")
men_total <- sum(men$ItemsOrdered)
men_total

women <- subset(ID86, ID86$Gender=="Female")
women_total <- sum(women$ItemsOrdered)
women_total


product_difference <- men_total- women_total
Q3_a <- product_difference
Q3_a           #men buy 267 less than the women 

#Question 3b
#Calculating the average for both men and women
avg_men <- mean(men$ItemsOrdered)
avg_men

avg_women <- mean(women$ItemsOrdered)
avg_women

#Average difference for both men and women
avg_difference <- avg_men-avg_women
avg_difference

#Converting the avg_difference to percentage
avg_difference*100           #The difference is significant.


#Question 3c
#Total purchased by men
men_purchase <- sum(men$TotalAmountPaid)
men_purchase


#Total purchased by women
women_purchased <- sum(women$TotalAmountPaid)
women_purchased

#Purchased difference for men and women
purchased_difference <- men_purchase - women_purchased
purchased_difference

#Question 4
#Tabulating the revenue by weekday
day_revenue <- aggregate(ID86$TotalAmountPaid, by=list(weekday=ID86$Day),FUN =sum)

highest_revenue <- subset(day_revenue, day_revenue$x==max(day_revenue$x))

highest_revenue$x     #The highest revenue

highest_revenue$weekday   #The day with the highest revenue is Saturday
