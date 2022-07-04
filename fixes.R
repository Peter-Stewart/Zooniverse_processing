# Dates/times are wrong for three sites where camera 15 was deployed, due to a fault with the camera
# Correct these using offset calculated from known deployment time

# Site 35
errors <- user_classifications %>% filter(site=="Site_35")
correct <- user_classifications %>% filter(site!="Site_35")

errors$DateTimeLub <- errors$DateTimeLub + 34443510 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

user_classifications <- rbind(errors, correct)

# Dates are wrong for Site 69
# timestamp = 2020-01-21 15:09:37
# true time = 2021-02-23 15:11:00
# offset = 34,473,683 seconds
rm(errors); rm(correct)

errors <- user_classifications %>% filter(site=="Site_69")
correct <- user_classifications %>% filter(site!="Site_69")

errors$DateTimeLub <- errors$DateTimeLub + 34473683 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

user_classifications <- rbind(errors, correct)

# Dates are wrong for site 86
# timestamp = 2020-02-10 13:08:00
# true time = 2021-03-15 13:09:00
# offset = 34,473,660
rm(errors); rm(correct)

errors <- user_classifications %>% filter(site=="Site_86")
correct <- user_classifications %>% filter(site!="Site_86")

errors$DateTimeLub <- errors$DateTimeLub + 34473660 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

user_classifications <- rbind(errors, correct)

#hist(user_classifications$DateTimeLub, breaks=100) # Confirm that all date/times are now within the correct range
