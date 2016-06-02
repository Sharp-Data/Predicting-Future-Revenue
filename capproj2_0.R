Order_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/Sales by customer clean.csv", header=TRUE, sep=",", na.strings = "")


summary(or_df)or_df <- data.frame(Order_Report)
SKU_Sales_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/Monthly SKU Sales Report.csv", header=TRUE, sep=",", na.strings = "")
sku_df <- data.frame(SKU_Sales_Report)
library(tidyr)
library(dplyr)
str(or_df)
str(sku_df)
Company_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/hubspot-crm-companies.csv", header=TRUE, sep=",", na.strings = "")
c_df <- data.frame(Company_Report)
str(c_df)
c_df$Time.Zone <- NULL
c_df$Lifecycle.Stage <- NULL
na_count <- sapply(c_df, function(y) sum(length(which(is.na(y)))))
na_count
c_df$Account.Type <- NULL
c_df$About.Us <- NULL
c_df$Budget.Cycle <- NULL
c_df$Cost.of.Current.Desks..each. <- NULL
c_df$Existing.Customer <- NULL
c_df$External.Contact.Id <- NULL
c_df$External.Account.Id <- NULL
c_df$External.User.Id <- NULL
c_df$First.Deal.Created.Date <- NULL
c_df$How.many.desks.in.company. <- NULL
c_df$How.many.locations. <- NULL
c_df$HubSpot.Owner <- NULL
c_df$Last.Modified.Date <- NULL
c_df$Lead.Status <- NULL
c_df$Next.Activity.Date <- NULL
c_df$Recent.Deal.Close.Date <- NULL
c_df$Recent.Deal.Amount <- NULL
c_df$Test.Desk.Issued. <- NULL
c_df$Total.Revenue <- NULL
c_df$Type <- NULL
c_df$What.Wellness.Initiatives.Do.You.Offer. <- NULL
c_df$Facebook.Fans  <- NULL
c_df$Google.Plus.Page <- NULL
c_df$Twitter.Bio  <- NULL
c_df$Twitter.Followers <- NULL
c_df$Original.Source.Data.2 <- NULL
c_df$Description <- NULL
c_df$LinkedIn.Bio <- NULL

#changing Number.of.Pageviews NA to 0
num_pviews_vec <- c_df$Number.of.Pageviews
num_pviews_vec <- ifelse(is.na(num_pviews_vec), 0, num_pviews_vec)
c_df <- mutate(c_df, Number.of.Pageviews = num_pviews_vec)

library(plyr)
summary(or_df)
or_df <- mutate(or_df, Orders = 1)
or_df <- ddply(or_df, .(Domain), mutate, Order_Number = cumsum(Orders))
or_df$Order <- NULL

or_num_df1 <- or_df
or_num_df1$Orders <- NULL
or_num_df1$Order <- NULL
or_num_df1$day <- NULL
or_num_df1$year <- NULL
or_num_df1$email <- NULL
or_num_df1$Order_Numbers <- NULL


Ord_Num_vec <- or_df$Order_Number
Ord_Num_vec1 <- ifelse((Ord_Num_vec == 1), c("Order_One_Amount"), c("Unknown"))
Ord_Num_vec2 <- ifelse((Ord_Num_vec == 2), c("Order_Two_Amount"), Ord_Num_vec1)
Ord_Num_vec3 <- ifelse((Ord_Num_vec == 3), c("Order_Three_Amount"), Ord_Num_vec2)
Ord_Num_vec4 <- ifelse((Ord_Num_vec == 4), c("Order_Four_Amount"), Ord_Num_vec3)
Ord_Num_vec5 <- ifelse((Ord_Num_vec == 5), c("Order_Five_Amount"), Ord_Num_vec4)
Ord_Num_vec6 <- ifelse((Ord_Num_vec == 6), c("Order_Six_Amount"), Ord_Num_vec5)
Ord_Num_vec7 <- ifelse((Ord_Num_vec == 7), c("Order_Seven_Amount"), Ord_Num_vec6)
Ord_Num_vec8 <- ifelse((Ord_Num_vec == 8), c("Order_Eight_Amount"), Ord_Num_vec7)
Ord_Num_vec9 <- ifelse((Ord_Num_vec == 9), c("Order_Nine_Amount"), Ord_Num_vec8)
Ord_Num_vec10 <- ifelse((Ord_Num_vec == 10), c("Order_Ten_Amount"), Ord_Num_vec9)
Ord_Num_vec11 <- ifelse((Ord_Num_vec == 11), c("Order_Eleven_Amount"), Ord_Num_vec10)
Ord_Num_vec12 <- ifelse((Ord_Num_vec == 12), c("Order_Twelve_Amount"), Ord_Num_vec11)
Ord_Num_vec13 <- ifelse((Ord_Num_vec == 13), c("Order_Thirteen_Amount"), Ord_Num_vec12)
Ord_Num_vec14 <- ifelse((Ord_Num_vec == 14), c("Order_Fourteen_Amount"), Ord_Num_vec13)
or_num_df1 <- mutate(or_num_df1, Order_Number = Ord_Num_vec14)

or_num_df1 <- spread(or_num_df1, Order_Number, total_sales, fill = 0)
or_num_df1 <- mutate(or_num_df1, total_revenue = Order_One_Amount + Order_Two_Amount + Order_Three_Amount + Order_Four_Amount + Order_Five_Amount + Order_Six_Amount + Order_Seven_Amount + Order_Eight_Amount + Order_Nine_Amount + Order_Ten_Amount + Order_Eleven_Amount + Order_Twelve_Amount + Order_Thirteen_Amount + Order_Fourteen_Amount)

c_df <- mutate(c_df, Domain = Company.Domain.Name)
c_df$Company.Domain.Name <- NULL
company_df <- left_join(or_num_df1, c_df, by = "Domain")

Order_One_Amount_vec <- company_df$Order_One_Amount
Order_One_Count_vec <- ifelse((Order_One_Amount_vec != 0), 1, 0)
Order_One_Count_vec

Order_Two_Amount_vec <- company_df$Order_Two_Amount
Order_Two_Count_vec <- ifelse((Order_Two_Amount_vec != 0), 1, 0)
Order_Two_Count_vec

Order_Three_Amount_vec <- company_df$Order_Three_Amount
Order_Three_Count_vec <- ifelse((Order_Three_Amount_vec != 0), 1, 0)
Order_Three_Count_vec

Order_Four_Amount_vec <- company_df$Order_Four_Amount
Order_Four_Count_vec <- ifelse((Order_Four_Amount_vec != 0), 1, 0)
Order_Four_Count_vec

Order_Five_Amount_vec <- company_df$Order_Five_Amount
Order_Five_Count_vec <- ifelse((Order_Five_Amount_vec != 0), 1, 0)
Order_Five_Count_vec

Order_Six_Amount_vec <- company_df$Order_Six_Amount
Order_Six_Count_vec <- ifelse((Order_Six_Amount_vec != 0), 1, 0)
Order_Six_Count_vec

Order_Seven_Amount_vec <- company_df$Order_Seven_Amount
Order_Seven_Count_vec <- ifelse((Order_Seven_Amount_vec != 0), 1, 0)
Order_Seven_Count_vec

Order_Eight_Amount_vec <- company_df$Order_Eight_Amount
Order_Eight_Count_vec <- ifelse((Order_Eight_Amount_vec != 0), 1, 0)
Order_Eight_Count_vec

Order_Nine_Amount_vec <- company_df$Order_Nine_Amount
Order_Nine_Count_vec <- ifelse((Order_Nine_Amount_vec != 0), 1, 0)
Order_Nine_Count_vec

Order_Ten_Amount_vec <- company_df$Order_Ten_Amount
Order_Ten_Count_vec <- ifelse((Order_Ten_Amount_vec != 0), 1, 0)
Order_Ten_Count_vec

Order_Eleven_Amount_vec <- company_df$Order_Eleven_Amount
Order_Eleven_Count_vec <- ifelse((Order_Eleven_Amount_vec != 0), 1, 0)
Order_Eleven_Count_vec

Order_Twelve_Amount_vec <- company_df$Order_Twelve_Amount
Order_Twelve_Count_vec <- ifelse((Order_Twelve_Amount_vec != 0), 1, 0)
Order_Twelve_Count_vec

Order_Thirteen_Amount_vec <- company_df$Order_Thirteen_Amount
Order_Thirteen_Count_vec <- ifelse((Order_Thirteen_Amount_vec != 0), 1, 0)
Order_Thirteen_Count_vec

Order_Fourteen_Amount_vec <- company_df$Order_Fourteen_Amount
Order_Fourteen_Count_vec <- ifelse((Order_Fourteen_Amount_vec != 0), 1, 0)
Order_Fourteen_Count_vec

company_df <- mutate(company_df, total_order_count = Order_One_Count_vec + Order_Two_Count_vec + Order_Three_Count_vec + Order_Four_Count_vec + Order_Five_Count_vec + Order_Six_Count_vec + Order_Seven_Count_vec + Order_Eight_Count_vec + Order_Nine_Count_vec + Order_Ten_Count_vec + Order_Eleven_Count_vec + Order_Twelve_Count_vec + Order_Thirteen_Count_vec + Order_Fourteen_Count_vec)

company_df$Annual.Revenue <- NULL
company_df$Associated.Deals <- NULL
company_df$Owner.Assigned.Date <- NULL
company_df$First.Conversion <- NULL
company_df$First.Conversion.Date <- NULL
company_df$Phone.Number <- NULL


company_df <- filter(company_df, !is.na(Domain))

library(httpuv)
library(questionr)
name_vec <- company_df$Name
name_vec <- addNAstr(name_vec, value = "Unknown")
name_vec
company_df <- mutate(company_df, Name = name_vec)

company_df$Associated.Contacts <- ifelse(is.na(company_df$Associated.Contacts), 0, company_df$Associated.Contacts)
company_df$City <- addNAstr(company_df$City, value =  "Unknown")
company_df$Country <- addNAstr(company_df$Country, value =  "Unknown")
company_df$Industry <- addNAstr(company_df$Industry, value =  "Unknown")
company_df$Is.Public <- addNAstr(company_df$Is.Public, value =  "Unknown")
company_df$State.Region <- addNAstr(company_df$State.Region, value =  "Unknown")
company_df$Street.Address <- addNAstr(company_df$Street.Address, value =  "Unknown")
company_df$Facebook.Company.Page <- addNAstr(company_df$Facebook.Company.Page, value =  "Unknown")
company_df$Website.URL <- addNAstr(company_df$Website.URL, value =  "Unknown")
company_df$LinkedIn.Company.Page <- addNAstr(company_df$LinkedIn.Company.Page, value =  "Unknown")
company_df$Twitter.Handle <- addNAstr(company_df$Twitter.Handle, value =  "Unknown")
company_df$Original.Source.Data.1 <- addNAstr(company_df$Original.Source.Data.1, value =  "Unknown")
company_df$Original.Source.Type <- addNAstr(company_df$Original.Source.Type, value =  "Unknown")
company_df$Postal.Code <- addNAstr(company_df$Postal.Code, value =  "Unknown")
company_df$Year.Founded <- addNAstr(company_df$Year.Founded, value =  "Unknown")
company_df$Last.Activity.Date <- addNAstr(company_df$Last.Activity.Date, value =  "Unknown")
company_df$Create.Date <- addNAstr(company_df$Create.Date, value =  "Unknown")
company_df$First.Contact.Create.Date <- addNAstr(company_df$First.Contact.Create.Date, value =  "Unknown")
company_df$Last.Contacted <- addNAstr(company_df$Last.Contacted, value =  "Unknown")
company_df$Number.of.Employees <- addNAstr(company_df$Number.of.Employees, value =  "Unknown")
company_df$Total.Money.Raised <- addNAstr(company_df$Total.Money.Raised, value =  "Unknown")
company_df$Days.to.Close <- addNAstr(company_df$Days.to.Close, value =  "Unknown")
company_df$Time.First.Seen <- addNAstr(company_df$Time.First.Seen, value =  "Unknown")
company_df$Time.Last.Seen <- addNAstr(company_df$Time.Last.Seen, value =  "Unknown")
company_df$Time.of.Last.Session <- addNAstr(company_df$Time.of.Last.Session, value =  "Unknown")
company_df$Time.of.First.Visit <- addNAstr(company_df$Time.of.First.Visit, value =  "Unknown")





company_df$Number.of.Form.Submissions <- ifelse(is.na(company_df$Number.of.Form.Submissions), 0, company_df$Number.of.Form.Submissions)
company_df$Number.of.Visits <- ifelse(is.na(company_df$Number.of.Visits), 0, company_df$Number.of.Visits)
company_df$Number.of.Pageviews <- ifelse(is.na(company_df$Number.of.Pageviews), 0, company_df$Number.of.Pageviews)




company_df$Street.Address.2 <- NULL
company_df$Recent.Conversion <- NULL
company_df$Recent.Conversion.Date <- NULL
company_df$Original.Source.Data.1 <- NULL
company_df$Original.Source.Type <- NULL

company_df$Name <- as.character(company_df$Name)
company_df$City <- as.character(company_df$City)
company_df$Country <- as.character(company_df$Country)
company_df$Industry <- as.character(company_df$Industry)
company_df$Number.of.Employees <- as.character(company_df$Number.of.Employees)
company_df$Postal.Code <- as.character(company_df$Postal.Code)
company_df$Street.Address <- as.character(company_df$Street.Address)
company_df$State.Region <- as.character(company_df$State.Region)

company_df$Domain <- factor(company_df$Domain)


na_count1 <- sapply(subdf, function(y) sum(length(which(is.na(y)))))
na_count1
head(subdf, n = 100L)
subdf$total_order_count  



nrow(company_df$total_revenue)

subdf <- slice(company_df, 700:1500)

str(subdf)

subdf$Is.Public <- NULL
subdf$Create.Date <- NULL
subdf$First.Contact.Create.Date <- NULL
subdf$Close.Date <- NULL
subdf$Last.Activity.Date  <- NULL
subdf$Last.Contacted <- NULL
subdf$Time.First.Seen    <- NULL
subdf$Time.Last.Seen <- NULL
subdf$Time.of.Last.Session <- NULL
subdf$Time.of.First.Visit <- NULL
subdf$Year.Founded <- NULL
subdf$Website.URL <- NULL
subdf$Facebook.Company.Page  <- NULL
subdf$LinkedIn.Company.Page <- NULL
subdf$Twitter.Handle <- NULL
subdf$Days.to.Close <- NULL

lm.1 <- lm(total_revenue~., data = subdf)
summary(lm.1, n = 50L)
str(lm.1)

subdf$Total.Money.Raised <- NULL
subdf$Postal.Code <- NULL
subdf$Street.Address <- NULL
subdf$State.Region <- NULL
subdf$City <- NULL
subdf$Country <- NULL
subdf$Industry <- NULL
subdf$Number.of.Employees <- NULL
subdf$Domain <- NULL
str(subdf)


# Algorythm
x <- model.matrix(total_revenue~., data = subdf)[, -1]       
y <- subdf$total_revenue                                                    

library(glmnet)
grid = 10^seq(10, -2, length = 100)
lasso.train <- glmnet(x, y, family = "binomial", alpha = 1, lambda = grid)
dim(coef(lasso.train))

set.seed(123)
cv.out = cv.glmnet(x,y,alpha=1,family="binomial")
plot(cv.out)
bestlam = cv.out$lambda.min

train.lasso = predict(lasso.train, s=bestlam, newx=x, type="class")

mean((train.lasso - y)^2)

out = glmnet(x,y,alpha=1,lambda=grid,family="binomial")
lasso.coef = predict(lasso.train,type="coefficients",s=bestlam)[1:56,]  
lasso.coef
lasso.coef[lasso.coef!=0] # n-var
length(lasso.coef[lasso.coef!=0])






#x.test <- model.matrix(hotel_cluster~., data=raw.test)[,-1]
#y.test <- raw.test$hotel_cluster

library(glmnet)
grid = 10^seq(10,-2,length=100)
lasso.train <- glmnet(x,y,family="binomial", alpha=1, lambda=grid)
dim(coef(lasso.train))

set.seed(123)
cv.out = cv.glmnet(x,y,alpha=1,family="binomial")
plot(cv.out)
bestlam = cv.out$lambda.min

#bestlam = cv.out$lambda.1se

# Training Accuracy
train.lasso = predict(lasso.train, s=bestlam, newx=x, type="class")

mean((train.lasso - y)^2)


# Test Accuracy
lasso.pred = predict(lasso.train, s=bestlam, newx=x.test, type="class")


out = glmnet(x,y,alpha=1,lambda=grid,family="binomial")
lasso.coef = predict(lasso.train,type="coefficients",s=bestlam)[1:58,]  #change the 58 to the number of vars
lasso.coef
lasso.coef[lasso.coef!=0] # n-var
length(lasso.coef[lasso.coef!=0])



#Below Code is for reference only
apply(your.matrix, 2, function(c)sum(c!=0))

x <- model.matrix(hotel_cluster~.,data=raw.train)[,-1]         # Change the hotel_cluster to the target variable; change the data reference
y <- raw.train$hotel_cluster                                                     # Change the hotel_cluster to the target variable; change the data reference


#x.test <- model.matrix(hotel_cluster~., data=raw.test)[,-1]
#y.test <- raw.test$hotel_cluster

library(glmnet)
grid = 10^seq(10,-2,length=100)
lasso.train <- glmnet(x,y,family="binomial", alpha=1, lambda=grid)
dim(coef(lasso.train))

set.seed(123)
cv.out = cv.glmnet(x,y,alpha=1,family="binomial")
plot(cv.out)
bestlam = cv.out$lambda.min

#bestlam = cv.out$lambda.1se

# Training Accuracy
train.lasso = predict(lasso.train, s=bestlam, newx=x, type="class")

mean((train.lasso - y)^2)


# Test Accuracy
lasso.pred = predict(lasso.train, s=bestlam, newx=x.test, type="class")


out = glmnet(x,y,alpha=1,lambda=grid,family="binomial")
lasso.coef = predict(lasso.train,type="coefficients",s=bestlam)[1:58,]  #change the 58 to the number of vars
lasso.coef
lasso.coef[lasso.coef!=0] # n-var
length(lasso.coef[lasso.coef!=0])










head(or_num_df)
str(or_num_df1)
str(c_df)
str(company_df)
head(subdf, n = 100L)
or_num_df <- rename(or_num_df, Order_One = 1)

or_df <- mutate(or_df, Order = "Order")
or_df <- unite(or_df, "Order_Numbers", "Order", "Order_Number", sep = " ", remove = FALSE)








?unite()

?spread()

or_df1 <- or_df
or_df1$email <- NULL
or_df1$year <- NULL
or_df1$day <- NULL
or_df1 <- ddply(or_df1, .(Domain), mutate, Total_Orders = cumsum(Orders), 
                Total_Sales = cumsum(total_sales))

summary(subdf)
str(or_df1)

or_df_test <- ddply(or_df, .(Domain))
summary(or_df_test)


aggregate(or_df, Domain)


summary(c_df)
head(or_df)
str(or_num_df)
str(c_df)

library(ggplot2)

ggplot(or_df, aes(total_sales, year, col = traffic_source)) +
  geom_point()




or_df$name <- NULL
or_df$order_count <- NULL
or_df_new <- spread(or_df, year, total_sales, 1:4, drop = FALSE) 
str(or_df_new)
str(or_df)



standdesk_df <- arrange(company_df, desc(total_revenue))
head(standdesk_df)
write.csv(standdesk_df, file = "companiesanddeals.csv", row.names=FALSE)















