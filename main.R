
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
  colnames(average_product_price)[names(average_product_price)=="gmv_eur_fixed"] <- "Actual selling price"
  colnames(average_product_price)[names(average_product_price)=="gmv_per_user"] <- "MV per user"

  listing_price <- aggregate(listing_price ~ Group, listing[!is.na(listing$listing_price),], FUN = mean )
  suggested_price <- aggregate(suggested_price ~ Group, listing[!is.na(listing$suggested_price),], FUN = mean )
  colnames(listing_price)[names(listing_price)=="listing_price"] <- "Listing price"
  colnames(suggested_price)[names(suggested_price)=="suggested_price"] <- "Suggested price"
  
  average_product_price <- left_join(average_product_price,listing_price)
  average_product_price <- left_join(average_product_price,suggested_price)
  
  avg_melt <- melt(average_product_price, id = 1)
  
  
  ggplot(avg_melt, aes(fill=Group, y=value, x=variable)) +
    geom_bar(position="dodge", stat="identity") +
    xlab("Variable")+
    ylab("Amount in eur") +
    ggtitle("Prices & MV")
  

# Seller perspective ------------------------------------------------------

sellers <- aggregate(user_id ~ Group, data = listing[!duplicated(listing$user_id),], FUN = length)
colnames(sellers)[2] <- "sellers_count"
sellers <- left_join(sellers, gmv_test[,c("Group","gmv_eur_fixed","gmv_per_user","user_id")])
colnames(sellers)[names(sellers)=="user_id"] <- "user_count"
items_posted <- aggregate(item_id ~ Group, data = listing, FUN = length)
colnames(items_posted)[2] <- "items_posted"
items_sold <- aggregate(item_id ~ Group, data = listing[!is.na(listing$gmv_eur_fixed),], FUN = length)
colnames(items_sold)[2] <- "items_sold"
sellers <- left_join(sellers,items_sold)
sellers <- left_join(sellers,items_posted)
sellers$items_sold_per_user <- sellers$items_sold/sellers$user_count
sellers$items_sold_per_seller <- sellers$items_sold/sellers$sellers_count
sellers$items_posted_per_user <- sellers$items_posted/sellers$user_count
sellers$items_posted_per_seller <- sellers$items_posted/sellers$sellers_count
sellers$gmv_per_seller <- sellers$gmv_eur_fixed/sellers$sellers_count
sellers$items_sold_per_items_posted_per_seller <- sellers$items_sold_per_seller/sellers$items_posted_per_seller
sellers$users_per_seller <- sellers$user_count/sellers$sellers_count

sellers_graph_data <- sellers[,c("Group","items_sold_per_seller","items_posted_per_seller")]
colnames(sellers_graph_data)[names(sellers_graph_data)=="items_sold_per_seller"] <- "Items sold per seller"
colnames(sellers_graph_data)[names(sellers_graph_data)=="items_posted_per_seller"] <- "Items posted per seller"
sell_melt <- melt(sellers_graph_data, id = 1)
ggplot(sell_melt, aes(fill=Group, y=value, x=variable)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Variable")+
  ylab("Units") +
  ggtitle("Items posted & sold per seller")

sellers_graph_data2 <- sellers[,c("Group","items_sold_per_user","items_posted_per_user")]
colnames(sellers_graph_data2)[names(sellers_graph_data2)=="items_sold_per_user"] <- "Items sold per user"
colnames(sellers_graph_data2)[names(sellers_graph_data2)=="items_posted_per_user"] <- "Items posted per user"
sell_melt2 <- melt(sellers_graph_data2, id = 1)
ggplot(sell_melt2, aes(fill=Group, y=value, x=variable)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Variable")+
  ylab("Units") +
  ggtitle("Items posted & sold per user")


# Impact on value added services ------------------------------------------


vas_use1 <- aggregate(revenue ~ Group + service_order_type, data = value_added_service_revenue, FUN = length )
vas_use1 <- left_join(vas_use1,user_count)
vas_use1$vas_per_1000_users <- vas_use1$revenue/vas_use1$user_id*1000

ggplot(vas_use1, aes(fill=Group, y=vas_per_1000_users, x=service_order_type)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Service type")+
  ylab("Value Added Services used per 1000 users, units") +
  ggtitle("Use of Value Added Services")

listing[listing$user_id %in% value_added_service_revenue$user_id,"vas"] <- "VAS used"
listing[is.na(listing$vas),"vas"] <- "VAS not used"


vas_use2 <- aggregate(user_id ~ Group + vas, data = listing[!is.na(listing$gmv_eur_fixed),], FUN = length )
vas_use3 <- aggregate(user_id ~ Group + vas, data = listing[!duplicated(listing$user_id),], FUN = length )
colnames(vas_use3)[3] <- "user_count"
vas_use2 <- left_join(vas_use2,vas_use3)
vas_use2$items_sold_per_user <- vas_use2$user_id/vas_use2$user_count

ggplot(vas_use2, aes(fill=Group, y=items_sold_per_user, x=vas)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Did seller use VAS on any of his/her items?")+
  ylab("Items sold per user, units") +
  ggtitle("Items sold by VAS usage")

# Seller perspective SPLITS------------------------------------------------------

listing$user_n_listings <- as.character(cut(listing$lister_nth_listing,
                                                  breaks = c(0,7,22,66,36723)))
listing[listing$user_n_listings == "(0,7]","user_n_listings"] <- "<7"
listing[listing$user_n_listings == "(7,22]","user_n_listings"] <- "7-22"
listing[listing$user_n_listings == "(22,66]","user_n_listings"] <- "23-66"
listing[listing$user_n_listings == "(66,3.67e+04]","user_n_listings"] <- ">66"

listing$user_n_listings <- factor(listing$user_n_listings, levels = c("<7","7-22","23-66",">66"))


sellers <- aggregate(user_id ~ Group + user_n_listings, data = listing[!duplicated(listing$user_id),], FUN = length)
colnames(sellers)[3] <- "sellers_count"

gmv_test <- aggregate(gmv_eur_fixed ~ Group + user_n_listings, data = listing, FUN = sum )
ab_test_users <- left_join(ab_test_users,listing[,c("user_id","user_n_listings")])
user_count <- aggregate(user_id ~ Group + user_n_listings, data = ab_test_users, FUN = length )
gmv_test <- left_join(gmv_test,user_count)
gmv_test$gmv_per_user <- gmv_test$gmv_eur_fixed/gmv_test$user_id
gmv_test$revenue <- gmv_test$gmv_per_user*1000*0.006

sellers <- left_join(sellers, gmv_test[,c("Group","user_n_listings","gmv_eur_fixed","gmv_per_user","user_id")])
colnames(sellers)[names(sellers)=="user_id"] <- "user_count"
items_posted <- aggregate(item_id ~ Group + user_n_listings, data = listing, FUN = length)
colnames(items_posted)[3] <- "items_posted"
items_sold <- aggregate(item_id ~ Group + user_n_listings, data = listing[!is.na(listing$gmv_eur_fixed),], FUN = length)
colnames(items_sold)[3] <- "items_sold"
sellers <- left_join(sellers,items_sold)
sellers <- left_join(sellers,items_posted)
sellers$items_sold_per_user <- sellers$items_sold/sellers$user_count
sellers$items_sold_per_seller <- sellers$items_sold/sellers$sellers_count
sellers$items_posted_per_user <- sellers$items_posted/sellers$user_count
sellers$items_posted_per_seller <- sellers$items_posted/sellers$sellers_count
sellers$gmv_per_seller <- sellers$gmv_eur_fixed/sellers$sellers_count
sellers$items_sold_per_items_posted_per_seller <- sellers$items_sold_per_seller/sellers$items_posted_per_seller
sellers$users_per_seller <- sellers$user_count/sellers$sellers_count

sellers_graph_data <- sellers[,c("Group","user_n_listings","items_sold_per_seller")]
colnames(sellers_graph_data)[names(sellers_graph_data)=="items_sold_per_seller"] <- "value"
colnames(sellers_graph_data)[names(sellers_graph_data)=="user_n_listings"] <- "variable"

sellers_graph_data$variable <- factor(sellers_graph_data$variable, levels = c("<7","7-22","23-66",">66"))

ggplot(sellers_graph_data, aes(fill=Group, y=value, x=variable)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Sellers number of listings posted")+
  ylab("Units") +
  ggtitle("Items sold per seller by sellers experience")

