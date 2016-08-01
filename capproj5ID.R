# Importing Shopify order data
Order_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/Sales by customer 6.24wID1.csv", header=TRUE, sep=",", na.strings = "")
or_df <- data.frame(Order_Report)


library(tidyr)
library(dplyr)

# Importing Hubspot company data
Company_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/hubspot-crm-view-companies2016-06-24wIDclean1.csv", header=TRUE, sep=",", na.strings = "")
c_df <- data.frame(Company_Report)

# Eliminating unnecessary variables from c_df
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


# Aggregating each order date based on domain for order_date_df, as subset of or_df
or_day_vec <- as.Date(or_df$day, "%m/%d/%Y")
or_df <- mutate(or_df, day = or_day_vec)
or_df <- mutate(or_df, Orders = 1)
attach(or_df)
or_df <- or_df[order(or_df$Company.ID, or_df$day),]
or_do_df <- select(or_df, Company.ID, Orders)
detach(or_df)
or_do_df <- ddply(or_do_df, .(Company.ID), mutate, Order_Number = cumsum(Orders))
or_df <- mutate(or_df, Order_Number = or_do_df$Order_Number)
or_df$Orders <- NULL
or_df <- filter(or_df, Order_Number <= 14)

head(or_df)


order_date_df <- select(or_df, Company.ID, day, Order_Number)

Order_Date_vec <- order_date_df$Order_Number
Order_Date_vec1 <- ifelse((Order_Date_vec == 1), c("Order_One_Date"), c("Unknown"))
Order_Date_vec2 <- ifelse((Order_Date_vec == 2), c("Order_Two_Date"), Order_Date_vec1)
Order_Date_vec3 <- ifelse((Order_Date_vec == 3), c("Order_Three_Date"), Order_Date_vec2)
Order_Date_vec4 <- ifelse((Order_Date_vec == 4), c("Order_Four_Date"), Order_Date_vec3)
Order_Date_vec5 <- ifelse((Order_Date_vec == 5), c("Order_Five_Date"), Order_Date_vec4)
Order_Date_vec6 <- ifelse((Order_Date_vec == 6), c("Order_Six_Date"), Order_Date_vec5)
Order_Date_vec7 <- ifelse((Order_Date_vec == 7), c("Order_Seven_Date"), Order_Date_vec6)
Order_Date_vec8 <- ifelse((Order_Date_vec == 8), c("Order_Eight_Date"), Order_Date_vec7)
Order_Date_vec9 <- ifelse((Order_Date_vec == 9), c("Order_Nine_Date"), Order_Date_vec8)
Order_Date_vec10 <- ifelse((Order_Date_vec == 10), c("Order_Ten_Date"), Order_Date_vec9)
Order_Date_vec11 <- ifelse((Order_Date_vec == 11), c("Order_Eleven_Date"), Order_Date_vec10)
Order_Date_vec12 <- ifelse((Order_Date_vec == 12), c("Order_Twelve_Date"), Order_Date_vec11)
Order_Date_vec13 <- ifelse((Order_Date_vec == 13), c("Order_Thirteen_Date"), Order_Date_vec12)
Order_Date_vec14 <- ifelse((Order_Date_vec == 14), c("Order_Fourteen_Date"), Order_Date_vec13)
order_date_df <- mutate(order_date_df, Order_Number = Order_Date_vec14)

order_date_df <- spread(order_date_df, Order_Number, day)

order_date_df <- order_date_df[c("Company.ID", "Order_One_Date", "Order_Two_Date", "Order_Three_Date", 
                                 "Order_Four_Date", "Order_Five_Date", "Order_Six_Date", "Order_Seven_Date", 
                                 "Order_Eight_Date", "Order_Nine_Date", "Order_Ten_Date", "Order_Eleven_Date", 
                                 "Order_Twelve_Date", "Order_Thirteen_Date", "Order_Fourteen_Date")]

# Determining days between first and last order for order_date_df
Last_Order_Date_vec14 <- order_date_df$Order_Fourteen_Date - order_date_df$Order_One_Date
Last_Order_Date_vec13 <- order_date_df$Order_Thirteen_Date - order_date_df$Order_One_Date
Last_Order_Date_vec12 <- order_date_df$Order_Twelve_Date - order_date_df$Order_One_Date
Last_Order_Date_vec11 <- order_date_df$Order_Eleven_Date - order_date_df$Order_One_Date
Last_Order_Date_vec10 <- order_date_df$Order_Ten_Date - order_date_df$Order_One_Date
Last_Order_Date_vec9 <- order_date_df$Order_Nine_Date - order_date_df$Order_One_Date
Last_Order_Date_vec8 <- order_date_df$Order_Eight_Date - order_date_df$Order_One_Date
Last_Order_Date_vec7 <- order_date_df$Order_Seven_Date - order_date_df$Order_One_Date
Last_Order_Date_vec6 <- order_date_df$Order_Six_Date - order_date_df$Order_One_Date
Last_Order_Date_vec5 <- order_date_df$Order_Five_Date - order_date_df$Order_One_Date
Last_Order_Date_vec4 <- order_date_df$Order_Four_Date - order_date_df$Order_One_Date
Last_Order_Date_vec3 <- order_date_df$Order_Three_Date - order_date_df$Order_One_Date
Last_Order_Date_vec2 <- order_date_df$Order_Two_Date - order_date_df$Order_One_Date

Days_between_vec <- ifelse(is.na(Last_Order_Date_vec14), Last_Order_Date_vec13, Last_Order_Date_vec14)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec12, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec11, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec10, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec9, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec8, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec7, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec6, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec5, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec4, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec3, Days_between_vec)
Days_between_vec <- ifelse(is.na(Days_between_vec), Last_Order_Date_vec2, Days_between_vec)

order_date_df <- mutate(order_date_df, Days_Between_All_Orders = Days_between_vec)


# cleaning or_num_df1, a subset of or_df
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
or_num_df1$name <- NULL
or_num_df1$`Email Prefix` <- NULL
or_num_df1$order_count <- NULL


# Aggregating each order amount based on domain for or_num_df1
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

or_num_df1 <- spread(or_num_df1, Order_Number, total_sales, fill = 0)

or_num_df1 <- mutate(or_num_df1, total_revenue = Order_One_Amount + Order_Two_Amount + 
                       Order_Three_Amount + Order_Four_Amount + Order_Five_Amount + Order_Six_Amount + 
                       Order_Seven_Amount + Order_Eight_Amount + Order_Nine_Amount + Order_Ten_Amount + 
                       Order_Eleven_Amount + Order_Twelve_Amount + Order_Thirteen_Amount + Order_Fourteen_Amount)
str(company_df)


# creating company_df by joining c_df with or_num_df1 and then order_date_df
company_df <- inner_join(or_num_df1, c_df, by = "Company.ID")

company_df <- inner_join(company_df, order_date_df, by = "Company.ID")


# Adding 1st order traffic source to company_df
traffic_source_df <- select(or_df, Company.ID, Order_Number, traffic_source)
traffic_source_df <- filter(traffic_source_df, Order_Number == 1)
traffic_source_df <- mutate(traffic_source_df, First_Order_Traffic_Source = traffic_source)
traffic_source_df$Order_Number <- NULL
traffic_source_df$traffic_source <- NULL
company_df <- inner_join(company_df, traffic_source_df, by = "Company.ID")


# Creating total_order_count variable in company_df
Order_One_Amount_vec <- company_df$Order_One_Amount
Order_One_Count_vec <- ifelse((Order_One_Amount_vec != 0), 1, 0)

Order_Two_Amount_vec <- company_df$Order_Two_Amount
Order_Two_Count_vec <- ifelse((Order_Two_Amount_vec != 0), 1, 0)

Order_Three_Amount_vec <- company_df$Order_Three_Amount
Order_Three_Count_vec <- ifelse((Order_Three_Amount_vec != 0), 1, 0)

Order_Four_Amount_vec <- company_df$Order_Four_Amount
Order_Four_Count_vec <- ifelse((Order_Four_Amount_vec != 0), 1, 0)

Order_Five_Amount_vec <- company_df$Order_Five_Amount
Order_Five_Count_vec <- ifelse((Order_Five_Amount_vec != 0), 1, 0)

Order_Six_Amount_vec <- company_df$Order_Six_Amount
Order_Six_Count_vec <- ifelse((Order_Six_Amount_vec != 0), 1, 0)

Order_Seven_Amount_vec <- company_df$Order_Seven_Amount
Order_Seven_Count_vec <- ifelse((Order_Seven_Amount_vec != 0), 1, 0)

Order_Eight_Amount_vec <- company_df$Order_Eight_Amount
Order_Eight_Count_vec <- ifelse((Order_Eight_Amount_vec != 0), 1, 0)

Order_Nine_Amount_vec <- company_df$Order_Nine_Amount
Order_Nine_Count_vec <- ifelse((Order_Nine_Amount_vec != 0), 1, 0)

Order_Ten_Amount_vec <- company_df$Order_Ten_Amount
Order_Ten_Count_vec <- ifelse((Order_Ten_Amount_vec != 0), 1, 0)

Order_Eleven_Amount_vec <- company_df$Order_Eleven_Amount
Order_Eleven_Count_vec <- ifelse((Order_Eleven_Amount_vec != 0), 1, 0)

Order_Twelve_Amount_vec <- company_df$Order_Twelve_Amount
Order_Twelve_Count_vec <- ifelse((Order_Twelve_Amount_vec != 0), 1, 0)

Order_Thirteen_Amount_vec <- company_df$Order_Thirteen_Amount
Order_Thirteen_Count_vec <- ifelse((Order_Thirteen_Amount_vec != 0), 1, 0)

Order_Fourteen_Amount_vec <- company_df$Order_Fourteen_Amount
Order_Fourteen_Count_vec <- ifelse((Order_Fourteen_Amount_vec != 0), 1, 0)

company_df <- mutate(company_df, total_order_count = Order_One_Count_vec + Order_Two_Count_vec + 
                       Order_Three_Count_vec + Order_Four_Count_vec + Order_Five_Count_vec + Order_Six_Count_vec + 
                       Order_Seven_Count_vec + Order_Eight_Count_vec + Order_Nine_Count_vec + Order_Ten_Count_vec + 
                       Order_Eleven_Count_vec + Order_Twelve_Count_vec + Order_Thirteen_Count_vec + Order_Fourteen_Count_vec)


# Eliminating unnecessary variables from company_df
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


library(httpuv)
library(questionr)

# Replacing NA values for company_df
company_df$City <- addNAstr(company_df$City, value =  "Unknown")
company_df$Country <- addNAstr(company_df$Country, value =  "Unknown")
company_df$Industry <- addNAstr(company_df$Industry, value =  "Unknown")
company_df$Is.Public <- addNAstr(company_df$Is.Public, value =  "Unknown")
company_df$State.Region <- addNAstr(company_df$State.Region, value =  "Unknown")
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

# Creating Ave_Days_Between_Orders variable in company_df
company_df <- mutate(company_df, Ave_Days_Between_Orders = Days_Between_All_Orders / (total_order_count - 1))


#Creating contact_df from hubspot contacts spreadsheet
Contact_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/hubspot-crm-view-contacts2016-06-24wID2.csv", header=TRUE, sep=",", na.strings = "")
contact_df <- data.frame(Contact_Report)

# Prepping contact_df in order to aggregate several variables 
contact_df <- mutate(contact_df, Count = 1)
contact_df <- ddply(contact_df, .(Company.ID), mutate, Domain_Count = cumsum(Count))
contact_df <- filter(contact_df, Domain_Count <= 12)


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
Contact_Num_vec11 <- ifelse((Contact_Num_vec == 11), c("Contact_Eleven"), Contact_Num_vec10)
Contact_Num_vec12 <- ifelse((Contact_Num_vec == 12), c("Contact_Twelve"), Contact_Num_vec11)
contact_df <- mutate(contact_df, Contact_Count = Contact_Num_vec12)

str(emails_open_df)


# Creating Total_Emails_Delivered variable in company_df
contact_df$Emails.Delivered <- as.numeric(levels(contact_df$Emails.Delivered))[contact_df$Emails.Delivered]
contact_df$Emails.Delivered <- ifelse(is.na(contact_df$Emails.Delivered), 0, contact_df$Emails.Delivered)

emails_del_df <- select(contact_df, Company.ID, Emails.Delivered, Contact_Count)
emails_del_df <- spread(emails_del_df, Contact_Count, Emails.Delivered, fill = 0)
emails_del_df <- mutate(emails_del_df, Total_Emails_Delivered = Contact_One + Contact_Two + 
                          Contact_Three + Contact_Four + Contact_Five + Contact_Six + 
                          Contact_Seven + Contact_Eight + Contact_Nine + Contact_Ten)
emails_del_df <- select(emails_del_df, Company.ID, Total_Emails_Delivered)
company_df <- inner_join(company_df, emails_del_df, by = "Company.ID")
company_df$Emails.Delivered <- NULL



# Creating Total_Emails_Opened variable in company_df
contact_df$Emails.Opened <- as.numeric(levels(contact_df$Emails.Opened))[contact_df$Emails.Opened]
contact_df$Emails.Opened <- ifelse(is.na(contact_df$Emails.Opened), 0, contact_df$Emails.Opened)

emails_open_df <- select(contact_df, Company.ID, Emails.Opened, Contact_Count)
emails_open_df <- spread(emails_open_df, Contact_Count, Emails.Opened, fill = 0)
emails_open_df <- mutate(emails_open_df, Total_Emails_Opened = Contact_One + Contact_Two + 
                           Contact_Three + Contact_Four + Contact_Five + Contact_Six + 
                           Contact_Seven + Contact_Eight + Contact_Nine + Contact_Ten)
emails_open_df <- select(emails_open_df, Company.ID, Total_Emails_Opened)
company_df <- inner_join(company_df, emails_open_df, by = "Company.ID")



# Creating Total_Emails_Clicked variable in company_df
contact_df$Emails.Clicked <- as.numeric(levels(contact_df$Emails.Clicked))[contact_df$Emails.Clicked]
contact_df$Emails.Clicked <- ifelse(is.na(contact_df$Emails.Clicked), 0, contact_df$Emails.Clicked)


emails_clicked_df <- select(contact_df, Company.ID, Emails.Clicked, Contact_Count)
emails_clicked_df <- spread(emails_clicked_df, Contact_Count, Emails.Clicked, fill = 0)
emails_clicked_df <- mutate(emails_clicked_df, Total_Emails_Clicked = Contact_One + Contact_Two + 
                              Contact_Three + Contact_Four + Contact_Five + Contact_Six + 
                              Contact_Seven + Contact_Eight + Contact_Nine + Contact_Ten)
emails_clicked_df <- select(emails_clicked_df, Company.ID, Total_Emails_Clicked)
company_df <- inner_join(company_df, emails_clicked_df, by = "Company.ID")



# Creating Emails_Opened_Percent variable in company_df
company_df <- mutate(company_df, Emails_Opened_Percent = Total_Emails_Opened / Total_Emails_Delivered)
company_df$Emails_Opened_Percent <- ifelse(is.na(company_df$Emails_Opened_Percent), 0, company_df$Emails_Opened_Percent)

# Creating Emails_Clicked_Percent variable in company_df
company_df <- mutate(company_df, Emails_Clicked_Percent = Total_Emails_Clicked / Total_Emails_Delivered)
company_df$Emails_Clicked_Percent <- ifelse(is.na(company_df$Emails_Clicked_Percent), 0, company_df$Emails_Clicked_Percent)

# Creating Ave_Order_Amount variable in company_df
company_df <- mutate(company_df, Ave_Order_Amount = total_revenue / total_order_count)

# Creating days_since_last_order variable in company_df
company_df <- mutate(company_df, today = Sys.Date())
company_df <- mutate(company_df, days_since_last_order = today - Order_One_Date - Days_Between_All_Orders)

# Creating days_since_first_order variable in company_df
company_df$days_since_last_order <- ifelse(is.na(company_df$days_since_last_order), 
                                           company_df$today - company_df$Order_One_Date, company_df$days_since_last_order)
company_df <- mutate(company_df, days_since_first_order = today - Order_One_Date)

# Creating Ave_Reorder variable in company_df
company_df <- mutate(company_df, Ave_Reorder = (total_revenue - Order_One_Amount) / (total_order_count - 1))
company_df$Ave_Reorder <- ifelse(is.na(company_df$Ave_Reorder), 0, company_df$Ave_Reorder)


# Creating sub_company_df, a subset of company_df
sub_company_df <- filter(company_df, total_revenue > 2500)
sub_company_df$after_cutoff_date <- ifelse(sub_company_df$Order_One_Date > "2016-01-12", 1, 0)
sub_company_df <- filter(sub_company_df, after_cutoff_date == 1)

# Creating modelsub1, a linear regression model for total_revenue
modelsub1 <- lm(total_revenue ~ days_since_last_order + Industry + Total_Emails_Opened*Total_Emails_Delivered, data = sub_company_df)
summary(modelsub1)



df.ans <- predict(modelsub1, data=sub_company_df)
sqrt(mean((df.ans-sub_company_df$total_revenue)^2))
df.ans

library(ggplot2)

ggplot(data = sub_company_df, aes(total_revenue, days_since_last_order)) +
  geom_point(aes(color = total_order_count, size = total_order_count)) +
  geom_smooth(method = lm) +
  guides(color = "legend")

ggplot(data = sub_company_df, aes(total_revenue, Total_Emails_Delivered)) +
  geom_point(position = "jitter", aes(color = Total_Emails_Opened, size = Total_Emails_Opened)) +
  geom_smooth(method = lm) +
  guides(color = "legend")

ggplot(data = sub_company_df, aes(total_revenue, Total_Emails_Opened)) +
  geom_point(position = "jitter", aes(color = Total_Emails_Delivered, size = Total_Emails_Delivered)) +
  geom_smooth(method = lm) +
  guides(color = "legend")
