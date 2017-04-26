library(jsonlite)
yelpusers <- stream_in(file("./yelp_academic_dataset_user.json"))
head(yelpusers)

#subset data into data frame
users <- yelpusers[c(-2,-5,-10,-19,-20,-23)]
users <- data.frame(users)
head(users)

############### clean data all NA turned into zero ################### 
users$review_count[is.na(users$review_count)] <- 0
users$yelping_since[is.na(users$yelping_since)] <- 0
users$useful[is.na(users$useful)] <- 0
users$funny[is.na(users$funny)] <- 0
users$cool[is.na(users$cool)] <- 0
users$fans[is.na(users$fans)] <- 0
users$average_stars[is.na(users$average_stars)] <- 0
users$compliment_hot[is.na(users$compliment_hot)] <- 0
users$compliment_more[is.na(users$compliment_more)] <- 0
users$compliment_profile[is.na(users$compliment_profile)] <- 0
users$compliment_cute[is.na(users$compliment_cute)] <- 0
users$compliment_list[is.na(users$compliment_list)] <- 0
users$compliment_note[is.na(users$compliment_note)] <- 0
users$compliment_plain[is.na(users$compliment_plain)] <- 0
users$compliment_writer[is.na(users$compliment_writer)] <- 0
users$compliment_photos[is.na(users$compliment_photos)] <- 0

# write to csv for further use
write.csv(users, "./yUsersFull.csv", row.names=F)
