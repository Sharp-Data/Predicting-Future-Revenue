library(tidyr)
library(dplyr)

# Importing Hubspot company data
Company_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/hubspot-crm-view-companies2016-06-24wID.csv", header=TRUE, sep=",", na.strings = "")
cid_df <- data.frame(Company_Report)

# Importing Shopify order data
Order_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/Sales by customer 6.24.csv", header=TRUE, sep=",", na.strings = "")
orid_df <- data.frame(Order_Report)

orid_df <- separate(orid_df, email, c("Email Prefix", "Domain"), sep = "@")
orid_df <- filter(orid_df, total_sales > 0)
orid_df$Domain <- tolower(orid_df$Domain)

cid_df <- mutate(cid_df, Domain = Company.Domain.Name)
cid_df$Company.Domain.Name <- NULL
cid_df$Domain <- tolower(cid_df$Domain)

sub_cid_df <- select(cid_df, Domain, Company.ID)
orid_df <- inner_join(orid_df, sub_cid_df, by = "Domain")

Contact_Report <- read.csv(file = "C:/Users/GamingFoSho/Documents/wdR/hubspot-crm-view-contacts2016-06-24.csv", header=TRUE, sep=",", na.strings = "")
contactid_df <- data.frame(Contact_Report)

contactid_df <- separate(contactid_df, Email, c("Email Prefix", "Domain"), sep = "@")
contactid_df$Domain <- tolower(contactid_df$Domain)
contactid_df <- filter(contactid_df, Domain != "standdesk.co")
contactid_df <- inner_join(contactid_df, sub_cid_df, by = "Domain")


orid_df$name <- NULL
orid_df$`Email Prefix` <- NULL
orid_df$Domain <- NULL
orid_df$company <- NULL

cid_df$Name <- NULL 
cid_df$Street.Address <- NULL
cid_df$Website.URL <- NULL 
cid_df$Facebook.Company.Page <- NULL
cid_df$Google.Plus.Page <- NULL                    
cid_df$LinkedIn.Bio <- NULL 
cid_df$LinkedIn.Company.Page <- NULL 
cid_df$Twitter.Handle <- NULL  
cid_df$Domain <- NULL


contactid_df$First.Name <- NULL
contactid_df$Last.Name <- NULL
contactid_df$Company.Name <- NULL
contactid_df$'Email Prefix' <- NULL
contactid_df$Domain <- NULL
contactid_df$Phone.Number <- NULL
contactid_df$Street.Address <- NULL
contactid_df$Billing.Address.Line.1 <- NULL
contactid_df$Shipping.Address.Line.1 <- NULL 
contactid_df$ReferrerEmail <- NULL
contactid_df$IP.Address <- NULL 
contactid_df$Website.URL <- NULL 

write.csv(orid_df, file = "C:/Users/GamingFoSho/Documents/wdR/Sales by customer 6.24wID1.csv", row.names=FALSE)
write.csv(cid_df, file = "C:/Users/GamingFoSho/Documents/wdR/hubspot-crm-view-companies2016-06-24wIDclean1.csv", row.names=FALSE)
write.csv(contactid_df, file = "C:/Users/GamingFoSho/Documents/wdR/hubspot-crm-view-contacts2016-06-24wID2.csv", row.names=FALSE)
