
library(dplyr)
library(ggplot2)
library(reshape2)

ab_test_users <- read.csv("data/ab_test_users.csv")
listing1 <- read.csv("data/listing.csv")
user_register <- read.csv("data/user_register.csv")
value_added_service_revenue1 <- read.csv("data/value_added_service_revenue.csv")

# transform

ab_test_users$test_variant <- as.character(ab_test_users$test_variant)
ab_test_users[ab_test_users$test_variant=="off","test_variant"] <- "Test"
ab_test_users[ab_test_users$test_variant=="on","test_variant"] <- "Control"
colnames(ab_test_users)[names(ab_test_users)=="test_variant"] <- "Group"

# add user info to main data frames

listing <- left_join(listing1,ab_test_users[,c("Group","user_id")])
value_added_service_revenue <- left_join(value_added_service_revenue1,ab_test_users[,c("Group","user_id")])
listing <- left_join(listing,user_register)
value_added_service_revenue <- left_join(value_added_service_revenue,user_register)

# revenue from GMV per 1 000 users


gmv_test <- aggregate(gmv_eur_fixed ~ Group, data = listing, FUN = sum )
user_count <- aggregate(user_id ~ Group, data = ab_test_users, FUN = length )
gmv_test <- left_join(gmv_test,user_count)
gmv_test$gmv_per_user <- gmv_test$gmv_eur_fixed/gmv_test$user_id
gmv_test$revenue <- gmv_test$gmv_per_user*1000*0.006


ggplot(data=gmv_test, aes(x=Group, y=revenue)) +
  geom_bar(stat="identity", fill="steelblue") +
  ylab("Revenue from GMV in eur")
  theme_minimal()
  
# revenue from value added services per 1 000 users
  
  
  vas_use <- aggregate(revenue ~ Group + service_order_type, data = value_added_service_revenue, FUN = sum )
  vas_use <- left_join(vas_use,user_count)
  vas_use$revenue_per_1000_users <- vas_use$revenue/vas_use$user_id*1000
  
  
  ggplot(vas_use, aes(fill=Group, y=revenue_per_1000_users, x=service_order_type)) +
    geom_bar(position="dodge", stat="identity") +
    xlab("Service type")+
    ylab("Revenue per 1000 users in eur") +
    ggtitle("Revenue from value added services")
  
# days to sell
  
  days_to_sell <- aggregate(days_to_sell ~ Group, data = listing[!is.na(listing$gmv_eur_fixed),], FUN = mean )
  
# average product price & MV per user
  
  average_product_price <- aggregate(gmv_eur_fixed ~ Group, listing[!is.na(listing$gmv_eur_fixed),], FUN = mean )
  average_product_price <- left_join(average_product_price,gmv_test[,c("Group","gmv_per_user")])
  colnames(average_product_price)[names(average_product_price)=="gmv_eur_fixed"] <- "Average unit price"
  colnames(average_product_price)[names(average_product_price)=="gmv_per_user"] <- "MV per user"
  avg_melt <- melt(average_product_price, id = 1)

  
  ggplot(avg_melt, aes(fill=Group, y=value, x=variable)) +
    geom_bar(position="dodge", stat="identity") +
    xlab("Variable")+
    ylab("Amount in eur") +
    ggtitle("Average price & MV per user")
  