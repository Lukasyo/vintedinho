
library(dplyr)
library(ggplot2)

ab_test_users <- read.csv("data/ab_test_users.csv")
listing1 <- read.csv("data/listing.csv")
user_register <- read.csv("data/user_register.csv")
value_added_service_revenue1 <- read.csv("data/value_added_service_revenue.csv")

# add user info to main data frames

listing <- left_join(listing1,ab_test_users[,c("test_variant","user_id")])
value_added_service_revenue <- left_join(value_added_service_revenue1,ab_test_users[,c("test_variant","user_id")])
listing <- left_join(listing,user_register)
value_added_service_revenue <- left_join(value_added_service_revenue,user_register)

# revenue from GMV per 1 000 users

listing[is.na(listing$gmv_eur_fixed),"gmv_eur_fixed"] <- 0
gmv_test <- aggregate(gmv_eur_fixed ~ test_variant, data = listing, FUN = function(x) { mean(x) * 1000 } )
gmv_test$revenue <- gmv_test$gmv_eur_fixed/1000*6

ggplot(data=gmv_test, aes(x=test_variant, y=revenue)) +
  geom_bar(stat="identity", fill="steelblue") +
  ylab("Revenue from GMV in eur")
  theme_minimal()
  
# revenue from value added services per 1 000 users
  
  listing[is.na(listing$gmv_eur_fixed),"gmv_eur_fixed"] <- 0
  gmv_test <- aggregate(gmv_eur_fixed ~ test_variant, data = listing, FUN = function(x) { mean(x) * 1000 } )
  gmv_test$revenue <- gmv_test$gmv_eur_fixed/1000*6
  
  ggplot(data=gmv_test, aes(x=test_variant, y=revenue)) +
    geom_bar(stat="identity", fill="steelblue") +
    ylab("Revenue from GMV in eur")
  theme_minimal()
