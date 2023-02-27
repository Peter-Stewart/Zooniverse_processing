# Load packages
library(MASS)
library(dplyr)
library(tidyr)
library(lubridate)
library(rethinking)
library(stringr)

# Load data ####
# Opuntia data from camera trap deployments
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
opuntia_data1 <- read.csv("Cameras_opuntia_data_main.csv", header = TRUE)

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data/Sign_survey_data")
opuntia_data2 <- read.csv("Sign_survey_opuntia_data_main.csv", header = TRUE)

# Interaction data
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
interactions1 <- read.csv("Fieldseason1_interactions_results.csv", header = TRUE)
interactions2 <- read.csv("Fieldseason2_interactions_results.csv", header = TRUE)

# Site data
site_data <- read.csv("Cameras_site_data_main.csv", header = TRUE)

# Consensus classifications
setwd("C:/temp/Zooniverse/Oct22/processed")
consensus_classifications <- get(load("consensus_classifications.Rdata"))

# Analysing Opuntia size and fruit ####
# Get both datasets into same format ####
# Makd survey date variable for both datasets
opuntia_data1$Month <- ifelse(opuntia_data1$Deploy_month < 10, paste("0",opuntia_data1$Deploy_month,sep=""), opuntia_data1$Deploy_month)
opuntia_data1 <- opuntia_data1 %>% unite(DateNum, c("Deploy_year", "Month", "Deploy_day"), sep = "-", remove = FALSE)
opuntia_data1$DateLub <- as_date(opuntia_data1$DateNum)

opuntia_data2$Month <- ifelse(opuntia_data2$Month < 10, paste("0",opuntia_data2$Month,sep=""), opuntia_data2$Month)
opuntia_data2 <- opuntia_data2 %>% unite(DateNum, c("Year", "Month", "Day"), sep = "-", remove = FALSE)
opuntia_data2$DateLub <- as_date(opuntia_data2$DateNum)

# Remove columns we don't need for now
opuntia_data1 <- opuntia_data1 %>% 
                  select(-"Cam_ID",
                        -"DateNum",
                        -"Deploy_day",
                        -"Deploy_month",
                        -"Deploy_year",
                        -"Deploy_date",
                        -"Deploy_day_of_year",
                        -"Month")

opuntia_data2 <- opuntia_data2 %>% 
                  select(-"Replicate",
                        -"Quarter",
                        -"X",
                        -"Y",
                        -"DateNum",
                        -"Day",
                        -"Month",
                        -"Year",
                        -"Notes")

# Bind into one dataset
opuntia_data <- rbind(opuntia_data1,opuntia_data2)

# Change variable classes to factor where relevant, correct any inconsistent factor coding
opuntia_data$Site_ID <- as.factor(opuntia_data$Site_ID)
opuntia_data$Stand_ID <- as.factor(opuntia_data$Stand_ID)
opuntia_data$Under_tree <- ifelse(opuntia_data$Under_tree == "n" | opuntia_data$Under_tree == "N" | opuntia_data$Under_tree == "no", 0, opuntia_data$Under_tree)
opuntia_data$Under_tree <- ifelse(opuntia_data$Under_tree == "Y" | opuntia_data$Under_tree == "yes", 1, opuntia_data$Under_tree)
opuntia_data$Under_tree <- as.factor(opuntia_data$Under_tree)

# Remove NA's (from sites where no Opuntia present)
opuntia_data <- opuntia_data %>% filter(!is.na(opuntia_data$Under_tree))
opuntia_data <- opuntia_data %>% filter(!is.na(opuntia_data$Ripe_fruits))

# Split into stricta and engelmannii
stricta <- opuntia_data %>% filter(Species == "stricta")
engelmannii <- opuntia_data %>% filter(Species == "engelmannii")

# Prior predictive simulations ####
N <- 1000 # Number of iterations

# Proposed priors
alpha_prior <- rnorm(N, 0, 10)
beta_prior <- rnorm(N, 0, 0.1)
#phi_prior <- rexp(N, 1)

# Calculate lambda over xseq
xseq <- seq(from = min(opuntia_data$Max_height), to = max(opuntia_data$Max_height), by = 1)

# Visualise 
plot(NULL, xlim=range(xseq), ylim=c(0,730), xlab="Max. height (cm)", ylab="Number of fruits", 
     main = expression(alpha *" ~ Normal(0, 10),  " * beta * " ~ Normal(0, 0.01)"))

plot(NULL, xlim=c(-2,2), ylim=c(0,730), xlab="Max. height (cm)", ylab="Number of fruits", 
     main = expression(alpha *" ~ Normal(0, 10),  " * beta * " ~ Normal(0, 0.01)"))

for ( i in 1:N ) curve(exp( alpha_prior[i] + beta_prior[i]*(x)) ,
                       from=min(xseq) , to=max(xseq) , add=TRUE ,
                       col=col.alpha("black",0.2) )

for ( i in 1:N ) curve(exp( alpha_prior[i] + beta_prior[i]*(x)) ,
                       from=-2 , to=2 , add=TRUE ,
                       col=col.alpha("black",0.2) )

# Modelling relationship between size and fruiting ####
# Engelmannii, ripe
dlist <- list(N = as.integer(nrow(engelmannii)),
              fruit = as.integer(engelmannii$Unripe_fruit),
              height = as.numeric(engelmannii$Max_height),
              cochineal = as.numeric(engelmannii$Cochineal_.))

m1 <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/fruit_analysis/neg_bin_v2.stan",
            data = dlist,
            chains = 4,
            cores = 4,
            warmup = 2000,
            iter = 4000)

post <- extract.samples(m1)

# Plot posterior predictions vs. actual data to assess model fit
xseq <- seq(min(engelmannii$Max_height), max(engelmannii$Max_height), by = 0.1)

#####
lambda <- matrix(nrow=length(post$alpha), ncol = length(xseq))
for(i in 1:length(xseq)){
  lambda[,i] <- exp(post$alpha + post$beta_height*xseq[i])
}
lambda_mu <- apply(lambda, 2, median)
lambda_pi <- apply(lambda, 2, HPDI, prob = 0.95)

sim_obs <- matrix(nrow=length(post$alpha), ncol = length(xseq))
for(i in 1:length(xseq)){
  for(j in 1:length(post$alpha)){
    sim_obs[j,i] <- rnegbin(1, mu = lambda[j,i], theta = post$phi[j]) 
  }
}

sim_obs_pi <- apply(sim_obs, 2, HPDI, prob = 0.95)

plot(NULL, xlim = c(5,205), ylim=c(0,730), xlab="Max. height (cm)", ylab="Number of ripe fruits", main = substitute(paste(italic("O. engelmannii"))))
points(x = xseq, y = lambda_mu, type = "l")
shade(lambda_pi, xseq)
#points(x = xseq, y = lambda_mu2, type = "l", lty=2)
shade(sim_obs_pi, xseq)
points(x = engelmannii$Max_height, y = engelmannii$Unripe_fruit, pch=16)

# Analysing behavioural interactions ####
interactions <- rbind(interactions1, interactions2)

interactions <- interactions %>% select(-ImageQuality, 
                                        -DeleteFlag,
                                        -RelativePath,
                                        -Folder)

interactions$site <- interactions$File
interactions$site <- gsub('_part.*','',interactions$site)
interactions$site <- gsub('Site_','',interactions$site)
interactions$site <- as.integer(interactions$site)

interactions$Type <- as.factor(interactions$Type)
interactions$Confidence <- as.factor(interactions$Confidence)

# Create DateTime column
interactions <- interactions %>% separate(Date, into = c("Day", "Month", "Year"), sep = "-", remove = FALSE, convert = FALSE)
interactions$Month <- match(interactions$Month, month.abb)
interactions$Month2 <- as.character(interactions$Month)
interactions$Month2 <- ifelse(interactions$Month < 10, paste("0",interactions$Month2,sep=""), interactions$Month2)
interactions <- interactions %>% unite(DateNum, c("Year", "Month2", "Day"), sep = "-", remove = FALSE)
interactions <- interactions %>% unite(DateTimeNum, c("DateNum", "Time"), sep = " ", remove = FALSE)
interactions$DateTimeLub <- as_datetime(interactions$DateTimeNum)

# Dates/times are wrong for three sites where camera 15 was deployed, due to a fault with the camera
# Correct these using offset calculated from known deployment time

# Site 35 parts 1 and 2 (part 3 used cam 6 which is ok)
errors <- interactions %>% filter(grepl("Site_35_part1|Site_35_part2",File))
correct <- interactions %>% filter(!grepl("Site_35_part1|Site_35_part2",File))

errors$DateTimeLub <- errors$DateTimeLub + 34443510 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

interactions <- rbind(errors, correct)

# Dates are wrong for Site 69
# timestamp = 2020-01-21 15:09:37
# true time = 2021-02-23 15:11:00
# offset = 34,473,683 seconds
rm(errors); rm(correct)

errors <- interactions %>% filter(grepl("Site_69",File))
correct <- interactions %>% filter(!grepl("Site_69",File))

errors$DateTimeLub <- errors$DateTimeLub + 34473683 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

interactions <- rbind(errors, correct)

# Dates are wrong for site 86 part 1
# timestamp = 2020-02-10 13:08:00
# true time = 2021-03-15 13:09:00
# offset = 34,473,660
rm(errors); rm(correct)

errors <- interactions %>% filter(grepl("Site_86_part1",File))
correct <- interactions %>% filter(!grepl("Site_86_part1",File))

errors$DateTimeLub <- errors$DateTimeLub + 34473660 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

interactions <- rbind(errors, correct)

# Dates are wrong for site 86 parts 2 to 3
# timestamp = 2020-01-01 12:01:29
# true time = 2021-03-19 11:55:00
# offset = 38,274,811

rm(errors); rm(correct)

errors <- interactions %>% filter(grepl("Site_86_part2|Site_86_part3",File))
correct <- interactions %>% filter(!grepl("Site_86_part2|Site_86_part3",File))

errors$DateTimeLub <- errors$DateTimeLub + 38274811 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

interactions <- rbind(errors, correct)

# Make sure to only use dry season data for now
dry_season <- interval("2021-01-01", "2021-05-01")
interactions <- interactions %>% filter(DateTimeLub %within% dry_season)

# Group interactions by interaction ID. For each ID get highest confidence rating for each type of interaction.
Max_conf <- interactions %>% group_by(Interaction_ID, Type) %>% summarise(Max_conf_int = max(as.integer(Confidence)))
Max_conf$Max_conf <- as.factor(Max_conf$Max_conf_int)
levels(Max_conf$Max_conf) <- levels(interactions$Confidence)

interactions <- merge(interactions, Max_conf, by = c("Interaction_ID","Type"))

# Remove interactions where max confidence is "unsure" or type is "none"
interactions <- interactions %>% filter(Max_conf != "Unsure") %>%
  filter(Type != "None")

# Split 'notes' variable to deal with cases with multiple species and/or interactions
interactions$Notes <- replace(interactions$Notes, interactions$Notes=="",NA)

interactionsA <- interactions %>% filter(!is.na(Notes))
interactionsB <- interactions %>% filter(is.na(Notes))

interactionsA <- interactionsA %>% separate_rows(Notes, sep = ",") %>% 
  select(-Type) %>%
  separate(Notes, into = c("species", "Type"), sep = "-", remove = FALSE, convert = FALSE)

# Merge with consensus classifications to match interaction with species
consensus_classifications <- consensus_classifications %>%
  mutate(File = str_extract(metadata, regex("(Site)(.*)(jpg)", ignore_case = TRUE))) %>%
  filter(!is.na(species))

consensus_sp <- consensus_classifications %>% select(File, species)

interactionsB <- merge(interactionsB, consensus_sp, by = "File", all.x = TRUE)

#check2 <- interactionsB %>% group_by(File) %>% filter(n_distinct(species) > 1) %>% filter(Type != "None")

# Deal with cases where no consensus classification (species = NA)
# Create list of these files - open and classify in Timelapse 
interactions_X <- rbind(interactionsA, interactionsB)

missing_sp <- interactions_X %>% filter(species=="noanimalspresent"|is.na(species))
missing_sp$subfolder <- gsub("_IMG.*","",missing_sp$File)
missing_sp$subfolder <- gsub("_RCNX.*","",missing_sp$subfolder)
missing_sp$path <- paste0("Site_",missing_sp$site,"/",
                          missing_sp$subfolder,"/",
                          missing_sp$File)
missing_sp_filelist <- missing_sp$path

#write.table(missing_sp_filelist, file = "interactions_missing_sp_filelist.txt",
#           sep = "\t", col.names = FALSE, row.names = FALSE)

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
missing_sp_ids <- read.csv("interactions_missing_sp.csv", header = TRUE)

missing_sp_ids <- missing_sp_ids %>% separate(Notes, into = c("species", "Type"), sep = "-", remove = FALSE, convert = FALSE) %>%
  select(File, species)
colnames(missing_sp_ids) <- c("File", "species_id")

interactions_Y <- merge(interactions_X, missing_sp_ids, by = "File", all.x = TRUE)

interactions_Y <- interactions_Y %>% mutate(species = coalesce(species, species_id))

# Deal with cases where other animals are in background
# Currently there are none of these which have not been dealt with using the "notes" field


# Remove interactions which are "unsure" or type "none"
interactions_sub <- interactions %>% filter(Confidence != "Unsure") %>%
                                  filter(Type != "None")

interactions_sub2 <- interactions_sub %>% select(-Date, -Time)



# Make sure that all interactions have a species assigned

# Create variable for the species of Opuntia that is present at each site
