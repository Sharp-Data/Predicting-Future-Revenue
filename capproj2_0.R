Order_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/Sales by customer clean.csv", header=TRUE, sep=",", na.strings = "")
or_df <- data.frame(Order_Report)


SKU_Sales_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/Monthly SKU Sales Report.csv", header=TRUE, sep=",", na.strings = "")
sku_df <- data.frame(SKU_Sales_Report)


library(tidyr)
library(dplyr)


Company_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/hubspot-crm-companies.csv", header=TRUE, sep=",", na.strings = "")
c_df <- data.frame(Company_Report)






c_df$Time.Zone <- NULL
c_df$Lifecycle.Stage <- NULL
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

str(c_df)
na_count <- sapply(c_df, function(y) sum(length(which(is.na(y)))))
na_count


#changing Number.of.Pageviews NA to 0
num_pviews_vec <- c_df$Number.of.Pageviews
num_pviews_vec <- ifelse(is.na(num_pviews_vec), 0, num_pviews_vec)
c_df <- mutate(c_df, Number.of.Pageviews = num_pviews_vec)


library(plyr)


or_df$Domain <- tolower(or_df$Domain)
or_day_vec <- as.Date(or_df$day, "%m/%d/%Y")
or_df <- mutate(or_df, day = or_day_vec)
or_df <- mutate(or_df, Orders = 1)
or_df <- or_df[order(or_df$Domain, or_df$day),]
or_df <- ddply(or_df, .(Domain), mutate, Order_Number = cumsum(Orders))
or_df$Orders <- NULL


or_num_df1 <- or_df
or_num_df1$company <- NULL
or_num_df1$shipping_city <- NULL
or_num_df1$shipping_province <- NULL
or_num_df1$day <- NULL
or_num_df1$year <- NULL
or_num_df1$email <- NULL
or_num_df1$traffic_source <- NULL
or_num_df1$host <- NULL
or_num_df1$referrer <- NULL
str(or_num_df1)



Ord_Num_vec <- or_num_df1$Order_Number
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
str(or_num_df1)

or_num_df1 <- spread(or_num_df1, Order_Number, total_sales, fill = 0)
str(or_num_df1)


or_num_df1 <- mutate(or_num_df1, total_revenue = Order_One_Amount + Order_Two_Amount + Order_Three_Amount + Order_Four_Amount + Order_Five_Amount + Order_Six_Amount + Order_Seven_Amount + Order_Eight_Amount + Order_Nine_Amount + Order_Ten_Amount + Order_Eleven_Amount + Order_Twelve_Amount + Order_Thirteen_Amount + Order_Fourteen_Amount)
str(or_num_df1)
str(c_df)


c_df <- mutate(c_df, Domain = Company.Domain.Name)
c_df$Company.Domain.Name <- NULL
c_df$Domain <- tolower(c_df$Domain)
or_num_df1$Domain <- tolower(or_num_df1$Domain)
company_df <- inner_join(or_num_df1, c_df, by = "Domain")



str(company_df)
str(or_num_df1)
str(c_df)
str(or_df)
or_df$traffic_source


# Adding 1st order traffic source to company_df
traffic_source_df <- select(or_df, Domain, Order_Number, traffic_source)
traffic_source_df <- filter(traffic_source_df, Order_Number == 1)
traffic_source_df <- mutate(traffic_source_df, First_Order_Traffic_Source = traffic_source)
traffic_source_df$Order_Number <- NULL
traffic_source_df$traffic_source <- NULL
company_df <- inner_join(company_df, traffic_source_df, by = "Domain")

str(company_df)



# Creating total_order_count variable in company_df
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

str(company_df)
na_count <- sapply(company_df, function(y) sum(length(which(is.na(y)))))
na_count



company_df$Street.Address.2 <- NULL
company_df$Recent.Conversion <- NULL
company_df$Recent.Conversion.Date <- NULL
company_df$Original.Source.Data.1 <- NULL
company_df$Original.Source.Type <- NULL
company_df$Order_Fourteen_Amount <- NULL
company_df$Order_Thirteen_Amount <- NULL
company_df$Order_Twelve_Amount <- NULL
company_df$Order_Eleven_Amount <- NULL
company_df$Order_Ten_Amount <- NULL
company_df$Order_Nine_Amount <- NULL
company_df$Order_Eight_Amount <- NULL
company_df$Order_Seven_Amount <- NULL
company_df$Order_Six_Amount <- NULL
company_df$Annual.Revenue <- NULL
company_df$Associated.Deals <- NULL
company_df$Owner.Assigned.Date <- NULL
company_df$First.Conversion <- NULL
company_df$First.Conversion.Date <- NULL
company_df$Phone.Number <- NULL
company_df$Name <- NULL


str(company_df)
company_df$Domain
na_count <- sapply(company_df, function(y) sum(length(which(is.na(y)))))
na_count


library(httpuv)
library(questionr)


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

company_df$Associated.Contacts <- ifelse(is.na(company_df$Associated.Contacts), 0, company_df$Associated.Contacts)
company_df$Number.of.Form.Submissions <- ifelse(is.na(company_df$Number.of.Form.Submissions), 0, company_df$Number.of.Form.Submissions)
company_df$Number.of.Visits <- ifelse(is.na(company_df$Number.of.Visits), 0, company_df$Number.of.Visits)
company_df$Number.of.Pageviews <- ifelse(is.na(company_df$Number.of.Pageviews), 0, company_df$Number.of.Pageviews)

na_count <- sapply(company_df, function(y) sum(length(which(is.na(y)))))
na_count
str(company_df)

modeltest1 <- lm(total_revenue ~ Number.of.Visits, data = company_df)
summary(modeltest1)

#Creating contact_df from hubspot contacts spreadsheet
Contact_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/Contacts Clean.csv", header=TRUE, sep=",", na.strings = "")
contact_df <- data.frame(Contact_Report)


contact_df <- separate(contact_df, Email, c("Email Prefix", "Domain"), sep = "@")
contact_df$Domain <- tolower(contact_df$Domain)
contact_df <- mutate(contact_df, Count = 1)
contact_df <- ddply(contact_df, .(Domain), mutate, Domain_Count = cumsum(Count))

contact_df <- filter(contact_df, Domain_Count <= 10)

Contact_Num_vec <- contact_df$Domain_Count
Contact_Num_vec1 <- ifelse((Contact_Num_vec == 1), c("Contact_One"), c("Unknown"))
Contact_Num_vec2 <- ifelse((Contact_Num_vec == 2), c("Contact_Two"), Contact_Num_vec1)
Contact_Num_vec3 <- ifelse((Contact_Num_vec == 3), c("Contact_Three"), Contact_Num_vec2)
Contact_Num_vec4 <- ifelse((Contact_Num_vec == 4), c("Contact_Four"), Contact_Num_vec3)
Contact_Num_vec5 <- ifelse((Contact_Num_vec == 5), c("Contact_Five"), Contact_Num_vec4)
Contact_Num_vec6 <- ifelse((Contact_Num_vec == 6), c("Contact_Six"), Contact_Num_vec5)
Contact_Num_vec7 <- ifelse((Contact_Num_vec == 7), c("Contact_Seven"), Contact_Num_vec6)
Contact_Num_vec8 <- ifelse((Contact_Num_vec == 8), c("Contact_Eight"), Contact_Num_vec7)
Contact_Num_vec9 <- ifelse((Contact_Num_vec == 9), c("Contact_Nine"), Contact_Num_vec8)
Contact_Num_vec10 <- ifelse((Contact_Num_vec == 10), c("Contact_Ten"), Contact_Num_vec9)

contact_df <- mutate(contact_df, Contact_Count = Contact_Num_vec10)


emails_del_df <- select(contact_df, Domain, Emails.Delivered, Contact_Count)
emails_del_df <- spread(emails_del_df, Contact_Count, Emails.Delivered, fill = 0)
emails_del_df <- mutate(emails_del_df, Total_Emails_Delivered = Contact_One + Contact_Two + Contact_Three + Contact_Four + Contact_Five + Contact_Six + Contact_Seven + Contact_Eight + Contact_Nine + Contact_Ten)
emails_del_df <- select(emails_del_df, Domain, Total_Emails_Delivered)
company_df <- inner_join(company_df, emails_del_df, by = "Domain")
company_df$Emails.Delivered <- NULL

emails_open_df <- select(contact_df, Domain, Emails.Opened, Contact_Count)
emails_open_df <- spread(emails_open_df, Contact_Count, Emails.Opened, fill = 0)
emails_open_df <- mutate(emails_open_df, Total_Emails_Opened = Contact_One + Contact_Two + Contact_Three + Contact_Four + Contact_Five + Contact_Six + Contact_Seven + Contact_Eight + Contact_Nine + Contact_Ten)
emails_open_df <- select(emails_open_df, Domain, Total_Emails_Opened)
company_df <- inner_join(company_df, emails_open_df, by = "Domain")

emails_clicked_df <- select(contact_df, Domain, Emails.Clicked, Contact_Count)
emails_clicked_df <- spread(emails_clicked_df, Contact_Count, Emails.Clicked, fill = 0)
emails_clicked_df <- mutate(emails_clicked_df, Total_Emails_Clicked = Contact_One + Contact_Two + Contact_Three + Contact_Four + Contact_Five + Contact_Six + Contact_Seven + Contact_Eight + Contact_Nine + Contact_Ten)
emails_clicked_df <- select(emails_clicked_df, Domain, Total_Emails_Clicked)
company_df <- inner_join(company_df, emails_clicked_df, by = "Domain")

contact_state_df <- select(contact_df, Domain, State.Region, Contact_Count)
contact_state_df <- spread(contact_state_df, Contact_Count, State.Region)
contact_state_df <- select(contact_state_df, Domain, Contact_One)
contact_state_df$Contact_One <- addNAstr(contact_state_df$Contact_One, value =  "Unknown")
company_df <- inner_join(company_df, contact_state_df, by = "Domain")


company_df <- mutate(company_df, Emails_Opened_Percent = Total_Emails_Opened / Total_Emails_Delivered)
company_df$Emails_Opened_Percent <- ifelse(is.na(company_df$Emails_Opened_Percent), 0, company_df$Emails_Opened_Percent)

company_df <- mutate(company_df, Emails_Clicked_Percent = Total_Emails_Clicked / Total_Emails_Delivered)
company_df$Emails_Clicked_Percent <- ifelse(is.na(company_df$Emails_Clicked_Percent), 0, company_df$Emails_Clicked_Percent)

modeltest3 <- lm(total_revenue ~ Emails_Clicked_Percent, data = company_df)
summary(modeltest3)




subdf <- company_df


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
subdf$Total.Money.Raised <- NULL
subdf$Postal.Code <- NULL
subdf$Street.Address <- NULL
subdf$State.Region <- NULL
subdf$City <- NULL
subdf$Country <- NULL
subdf$Industry <- NULL
subdf$Number.of.Employees <- NULL
subdf$Order_Five_Amount <- NULL
subdf$Order_Four_Amount <- NULL
subdf$Order_Three_Amount <- NULL
subdf$Order_Two_Amount <- NULL
subdf$Order_One_Amount <- NULL

na_count <- sapply(subdf, function(y) sum(length(which(is.na(y)))))
na_count
str(company_df)

str(subdf)
subdf$Number.of.Pageviews

lm.1 <- lm(total_revenue~. -Domain, data = subdf)
summary(lm.1)
str(lm.1)

modeltest <- lm(total_revenue ~ Number.of.Visits, data = subdf)
summary(modeltest)















