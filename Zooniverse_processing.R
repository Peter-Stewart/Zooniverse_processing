# Load packages ####
library(dplyr)
library(tidyr)
library(lubridate)

# Load data ####
setwd("C:/temp/Zooniverse/June22")

workflows <- read.csv("prickly-pear-project-kenya-workflows.csv", header = TRUE)
subjects <- read.csv("prickly-pear-project-kenya-subjects.csv")
subjects <- subjects %>% filter(subject_set_id == 99701)
#classifications <- read.csv("prickly-pear-project-kenya-classifications.csv", header = TRUE)

setwd("C:/temp/Zooniverse/June22/extracted")
survey_extractions <- read.csv("survey_extractor_extractions.csv")

# Process survey extractions ####
# Make count into an integer
survey_extractions$count <- rep(NA, nrow(survey_extractions))
survey_extractions$count <- as.integer(rep(NA, nrow(survey_extractions)))
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.1==1)] = 1L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.2==1)] = 2L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.3==1)] = 3L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.4==1)] = 4L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.5==1)] = 5L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.6==1)] = 6L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.7==1)] = 7L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.8==1)] = 8L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.9==1)] = 9L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.10==1)] = 10L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.1150==1)] = 11L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.51==1)] = 51L

# Add variable indicating whether count is exact
survey_extractions$count_exact <- ifelse(survey_extractions$count > 10, FALSE, TRUE)

# Add index variable for "interacting with cactus"
survey_extractions$interacting <- as.integer(rep(NA, nrow(survey_extractions)))
survey_extractions$interacting[which(survey_extractions$data.answers_doyouseeanyindividualsinteractingwiththecactus.yes==1)] = 1
survey_extractions$interacting[which(survey_extractions$data.answers_doyouseeanyindividualsinteractingwiththecactus.no==1)] = 0

# Merge classification data with subject set data
survey_extractions <- survey_extractions %>% select(classification_id, 
                                                    user_name,
                                                    user_id,
                                                    workflow_id,
                                                    task,
                                                    created_at,
                                                    subject_id,
                                                    data.choice,
                                                    count,
                                                    count_exact,
                                                    interacting)

subjects_sub <- subjects %>% select(subject_id, 
                                    metadata, 
                                    subject_set_id, 
                                    retired_at, 
                                    retirement_reason,
                                    classifications_count)

user_classifications <- merge(subjects_sub, survey_extractions, by="subject_id", all = TRUE)

# Remove subjects with no metadata / classification ID, and those from the wrong subject sets
user_classifications <- user_classifications %>% filter(metadata != "NA") %>% filter(classification_id != "NA")
user_classifications <- user_classifications %>% filter(subject_set_id == 99701)

# Rename data.choice to "species"
user_classifications$species <- as.factor(user_classifications$data.choice)
user_classifications <- user_classifications %>% select(-data.choice)

# Add missing metadata for Site_31_part1 (missing due to error when uploading to Zooniverse)
# First get subject ID's and image names for the images with missing metadata
missingIDs <- subjects %>% filter(grepl("Site_31_part1",metadata)) %>% select(subject_id, metadata)
missingIDs$metadata <- gsub('\"Filename\":\"','',missingIDs$metadata)
missingIDs$metadata <- gsub("[{}]", "" ,missingIDs$metadata)
missingIDs$metadata <- gsub('"', "" ,missingIDs$metadata)
missingIDs$File <- missingIDs$metadata
missingIDs <- missingIDs %>% select(-metadata)

# Then load the manifest csv containing the info for the metadata, and merge with the subject ID's
setwd("C:/temp/Zooniverse/Feb22")
manifest <- read.csv("Site_31_part1_TimelapseData.csv", header = TRUE)
fix <- merge(missingIDs, manifest, by="File")

# Finally, reconstruct metadata from the information in the manifest
fix$metadata <- paste('{\"Date\":\"',
                  fix$Date,
                  '\",\"File\":\"',
                  fix$File,
                  '\",\"Time\":\"',
                  fix$Time,
                  '\",\"#Empty\":\"FALSE\",\"Folder\":\"', # All are Empty == FALSE so just paste in directly
                  fix$Folder,
                  '\",\"#Animal\":\"TRUE\",\"#Person\":\"FALSE\",\"#DeleteFlag\":\"FALSE\",\"#ImageQuality\":\"Ok\",\"#RelativePath\":\"\"}' # These are the same for all images so just paste in directly
                  )
fix$metadata <- gsub(" ","",fix$metadata) # Remove spaces in the metadata

# Merge the corrected metadata into the user_classifications dataframe
user_classifications$metadata <- ifelse(user_classifications$subject_id %in% fix$subject_id,
                                         fix$metadata,
                                         user_classifications$metadata)

# Also merge into the subjects_sub data frame
subjects_sub$metadata <- ifelse(subjects_sub$subject_id %in% fix$subject_id,
                                        fix$metadata,
                                        subjects_sub$metadata)


# Split metadata into separate columns, drop non-useful info. Retain original metadata column too.
user_classifications <- user_classifications %>% separate(metadata, 
                                                          into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", 
                                                                   "k", "l", "m"),
                                                          sep = ":", 
                                                          remove = FALSE,
                                                          convert = FALSE,
                                                          extra = "warn",
                                                          fill = "warn") %>%
  separate(b, into = c("Date", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  separate(c, into = c("File", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  separate(f, into = c("Sec", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  select(-a, -g, -h, -i, -j, -k, -l, -m) %>%
  rename(Hour = d) %>%
  rename(Min = e) 


user_classifications$Date <- gsub('"','',user_classifications$Date)
user_classifications$Hour <- gsub('"','',user_classifications$Hour)
user_classifications$Sec <- gsub('"','',user_classifications$Sec)

user_classifications$site <- user_classifications$File
user_classifications$site <- gsub('"','',user_classifications$site)
user_classifications$site <- gsub('.jpg','',user_classifications$site)
user_classifications$site <- gsub('.{9}$','',user_classifications$site)
user_classifications$site <- gsub("_part.*",'',user_classifications$site)
user_classifications$site <- as.factor(user_classifications$site)

user_classifications$File <- gsub('"','',user_classifications$File)
user_classifications$File <- gsub('.jpg','',user_classifications$File)

# Split date into day, month, year columns (keep original)
user_classifications <- user_classifications %>% separate(Date, into = c("Day", "Month", "Year"), sep = "-", remove = FALSE, convert = FALSE)

# Convert month and year to required format
user_classifications$Month <- match(user_classifications$Month, month.abb)
user_classifications$Month2 <- as.character(user_classifications$Month)
user_classifications$Month2 <- ifelse(user_classifications$Month < 10, paste("0",user_classifications$Month2,sep=""), user_classifications$Month2)
user_classifications$Year <- as.integer(paste("20",user_classifications$Year, sep = ""))

user_classifications <- user_classifications %>% unite(DateNum, c("Year", "Month2", "Day"), sep = "-", remove = FALSE)
user_classifications <- user_classifications %>% unite(TimeNum, c("Hour", "Min", "Sec"), sep = ":", remove = FALSE)
user_classifications <- user_classifications %>% unite(DateTimeNum, c("DateNum", "TimeNum"), sep = " ", remove = FALSE)

# Create datetime column
user_classifications$DateTimeLub <- as_datetime(user_classifications$DateTimeNum)
user_classifications$ClassLub <- as_datetime(user_classifications$created_at)

# Repeat for subjects_sub dataframe ####
# Split metadata into separate columns, drop non-useful info. Retain original metadata column too.
subjects_sub <- subjects_sub %>% separate(metadata, 
                                          into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", 
                                                   "k", "l", "m"),
                                          sep = ":", 
                                          remove = FALSE,
                                          convert = FALSE,
                                          extra = "warn",
                                          fill = "warn") %>%
  separate(b, into = c("Date", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  separate(c, into = c("File", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  separate(f, into = c("Sec", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  select(-a, -g, -h, -i, -j, -k, -l, -m) %>%
  rename(Hour = d) %>%
  rename(Min = e) 

subjects_sub$Date <- gsub('"','',subjects_sub$Date)
subjects_sub$Hour <- gsub('"','',subjects_sub$Hour)
subjects_sub$Sec <- gsub('"','',subjects_sub$Sec)

subjects_sub$site <- subjects_sub$File
subjects_sub$site <- gsub('"','',subjects_sub$site)
subjects_sub$site <- gsub('.jpg','',subjects_sub$site)
subjects_sub$site <- gsub('.{9}$','',subjects_sub$site)
subjects_sub$site <- gsub("_part.*",'',subjects_sub$site)
subjects_sub$site <- as.factor(subjects_sub$site)

subjects_sub$File <- gsub('"','',subjects_sub$File)
subjects_sub$File <- gsub('.jpg','',subjects_sub$File)

# Split date into day, month, year columns (keep original)
subjects_sub <- subjects_sub %>% separate(Date, into = c("Day", "Month", "Year"), sep = "-", remove = FALSE, convert = FALSE)

# Convert month and year to required format
subjects_sub$Month <- match(subjects_sub$Month, month.abb)
subjects_sub$Month2 <- as.character(subjects_sub$Month)
subjects_sub$Month2 <- ifelse(subjects_sub$Month < 10, paste("0",subjects_sub$Month2,sep=""), subjects_sub$Month2)
subjects_sub$Year <- as.integer(paste("20",subjects_sub$Year, sep = ""))

subjects_sub <- subjects_sub %>% unite(DateNum, c("Year", "Month2", "Day"), sep = "-", remove = FALSE)
subjects_sub <- subjects_sub %>% unite(TimeNum, c("Hour", "Min", "Sec"), sep = ":", remove = FALSE)
subjects_sub <- subjects_sub %>% unite(DateTimeNum, c("DateNum", "TimeNum"), sep = " ", remove = FALSE)

# Create datetime column
subjects_sub$DateTimeLub <- as_datetime(subjects_sub$DateTimeNum)


# Fixes ####
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

# Fixes for subjects_sub ####
# Dates/times are wrong for three sites where camera 15 was deployed, due to a fault with the camera
# Correct these using offset calculated from known deployment time

# Site 35
rm(errors); rm(correct)''
errors <- subjects_sub %>% filter(site=="Site_35")
correct <- subjects_sub %>% filter(site!="Site_35")

errors$DateTimeLub <- errors$DateTimeLub + 34443510 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

subjects_sub <- rbind(errors, correct)

# Dates are wrong for Site 69
# timestamp = 2020-01-21 15:09:37
# true time = 2021-02-23 15:11:00
# offset = 34,473,683 seconds
rm(errors); rm(correct)

errors <- subjects_sub %>% filter(site=="Site_69")
correct <- subjects_sub %>% filter(site!="Site_69")

errors$DateTimeLub <- errors$DateTimeLub + 34473683 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

subjects_sub <- rbind(errors, correct)

# Dates are wrong for site 86
# timestamp = 2020-02-10 13:08:00
# true time = 2021-03-15 13:09:00
# offset = 34,473,660
rm(errors); rm(correct)

errors <- subjects_sub %>% filter(site=="Site_86")
correct <- subjects_sub %>% filter(site!="Site_86")

errors$DateTimeLub <- errors$DateTimeLub + 34473660 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

subjects_sub <- rbind(errors, correct)

#hist(subjects_sub$DateTimeLub, breaks=100) # Confirm that all date/times are now within the correct range

# Generate consensus classifications ####
# Calculate number of votes for each species in each subject
sp_votes <- user_classifications %>% group_by(subject_id) %>% count(species, name = "votes") 

# Calculate total number of times each subject has been classified - NB that Zooniverse "classifications_count" variable is NOT accurate
user_classifications$classification_id <- as.factor(user_classifications$classification_id)
total_classifications <- user_classifications %>% group_by(subject_id) %>% count(classification_id) %>% count(subject_id, name = "n_classifications")
colnames(total_classifications) <- c("subject_id","total_subject_classifications")

#check <- merge(user_classifications, total_classifications, by="subject_id")
#check$v <- check$total_subject_classifications - check$classifications_count
#check2 <- check %>% select(subject_id, classification_id, species, total_subject_classifications, classifications_count, v) %>% filter(check$v !=0)

sp_votes <- merge(sp_votes,total_classifications, all.x = TRUE)

# Calculate votes for a species as proportion of total classifications
sp_votes$votes_prop <- sp_votes$votes/sp_votes$total_subject_classifications

# Accept classification if votes exceed threshold, warn if close to threshold
consensus_threshold <- 0.66
warning_threshold <- 0.45

sp_votes$classification_accept <- ifelse(sp_votes$votes_prop >= consensus_threshold, TRUE, FALSE)
sp_votes$warning_disagreement <- ifelse(consensus_threshold > sp_votes$votes_prop & sp_votes$votes_prop >= warning_threshold, TRUE, FALSE)

# calculate information entropy of votes and warn if entropy is high 
entropy_threshold <- 1
entropy_calc <- function(x){
  -sum(x*log(x))
}

entropy <- sp_votes %>% select(subject_id, votes_prop)
entropy <- aggregate(entropy, by = list(entropy$subject_id), FUN = entropy_calc)
entropy <- entropy %>% select(Group.1, votes_prop)
colnames(entropy) <- c("subject_id","entropy")
entropy$entropy_warning <- ifelse(entropy$entropy > entropy_threshold, TRUE, FALSE)

sp_votes <- merge(sp_votes, entropy, by = "subject_id", all.x = TRUE)

# Subset accepted classifications and merge with site data - drop columns which aren't needed
# Only take images with 12 or more classifications - ones with fewer have been flagged "human" and need expert check
accepted <- sp_votes %>% filter(total_subject_classifications > 11) %>% filter(classification_accept==TRUE)

consensus_classifications <- merge(accepted, subjects_sub, all=TRUE)
consensus_classifications <- consensus_classifications %>% select(
  subject_id,
  species,
  metadata,
  site,
  DateTimeLub
)

# AT THIS STAGE MERGE IN EXPERT CLASSIFICATIONS IF I MAKE THOSE EVENTUALLY


# Generate detection matrix for each species (number of detections per hour, can binarise if needed) ####
# Hours and days for all sites - will need these so that hours/days with no detections can be kept as zero
setwd("C:/temp/Zooniverse/June22")
startends <- read.csv("Fieldseason1_startends.csv")
startends$Days <- as.integer(startends$Days) + 1L # Add one or it doesn't count the deployment day!

startends$Deploy_month <- ifelse(startends$Deploy_month < 10, paste("0",startends$Deploy_month,sep=""), startends$Deploy_month)
startends <- startends %>% unite(Deploy_date_num, c("Deploy_year", "Deploy_month", "Deploy_day"), sep = "-", remove = FALSE)
startends$Deploy_date_lub <- as_date(startends$Deploy_date_num)

user_classifications$Month2 <- as.character(user_classifications$Month)
user_classifications$Month2 <- ifelse(user_classifications$Month < 10, paste("0",user_classifications$Month2,sep=""), user_classifications$Month2)
user_classifications$Year <- as.integer(paste("20",user_classifications$Year, sep = ""))

user_classifications <- user_classifications %>% unite(DateNum, c("Year", "Month2", "Day"), sep = "-", remove = FALSE)

hrs <- as.data.frame(0:23); colnames(hrs) <- "hr"
sitedays <- startends %>% select(Site, Days) %>% filter(!is.na(Days))

sites_k <- startends %>% select(Site, Deploy_date_lub, Days) %>% 
  filter(!is.na(Days)) %>%
  uncount(Days)
  

# Each row is one site





# Each row is one day at a site



# should be zero if surveyed but not seen, NA if not surveyed - so need to check how many days each camera was deployed for
# all columns should be zero, but some rows may be entirely NA - since in current format all hours are surveyed, but not all days
# can then just remove empty days? (days which are all NA) - or all 0 after converting NA to 0
# can always expand to deal with hours at end which aren't measured - fill with NA

# if changing to visits==days then will have more standard ragged array with NA for days which weren't surveyed

# can use time of deployment from sheet and time of last image (whether empty or not) to define start/end times


# need to source helper functions here
source("C:/Users/PeteS/OneDrive/R Scripts Library/Projects/Zooniverse/helper_functions_v1.R", echo = FALSE)

# Create (binary) detection matrix for each species
sp_list <- unique(levels(consensus_classifications$species))

detmats <- list()
for(i in 1:length(sp_list)){
  detmats[[i]] <- generate_detection_matrix_hours(sp=sp_list[i], binary=TRUE)
}
names(detmats) <- sp_list
