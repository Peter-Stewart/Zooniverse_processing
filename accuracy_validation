# Packages ####
library(caret)
library(dplyr)
library(forcats)
library(viridis)
library(cowplot)

# Load data ####
setwd("C:/temp/Zooniverse/Feb23/processed")
validation_set <- get(load("validation_set.Rdata"))
expert_usernames <- c("Peter.Stewart","Callie25")

# Subset to just essential variables
validation_sub <- validation_set %>% select(subject_id, species, user_id, gold)

# Variable which gives expert ID for each image
validation_sub$expert_match <- NA 

# Split into expert and volunteer classifications
expert_sub <- validation_sub %>% filter(gold == 1)
volunteer_sub <- validation_sub %>% filter(gold == 0)

# Expert subset contains  duplicates (e.g. where expert saw same image twice) - remove these
expert_sub <- expert_sub %>% distinct(subject_id, species, .keep_all = TRUE)

# Expert match = 1 if volunteer classification matches expert classification for that image
for(i in 1:nrow(volunteer_sub)){
  temp1 <- expert_sub %>% filter(subject_id == volunteer_sub$subject_id[i])
  
  if(volunteer_sub$species[i] %in% temp1$species){
    volunteer_sub$expert_match[i] <- 1
  }else{
    volunteer_sub$expert_match[i] <- 0
  }
}
expert_match <- rbind(expert_sub, volunteer_sub)

# Save the dataframe to avoid having to redo the calculation
setwd("C:/temp/Zooniverse/Feb23/processed")
save(expert_match, file ="expert_match.Rdata")
save(expert_sub, file = "expert_sub.Rdata")
save(volunteer_sub, file = "volunteer_sub.Rdata")

expert_match <- get(load("expert_match.Rdata"))
expert_sub <- get(load("expert_sub.Rdata"))
volunteer_sub <- get(load("volunteer_sub.Rdata"))  

# Generate pseudo-consensus classifications (actual consensus for these images was the expert classification!)
total_classifications <- validation_set %>% group_by(subject_id) %>% count(classification_id) %>% count(subject_id, name = "n_classifications")
matches_consensus <- validation_set %>% group_by(subject_id) %>% count(species, name = "votes") 
matches_consensus <- merge(matches_consensus, total_classifications, all.x = TRUE)
matches_consensus$votes_prop <- matches_consensus$votes/matches_consensus$n_classifications

consensus_threshold <- 0.66
warning_threshold <- 0.45

matches_consensus$classification_accept <- ifelse(matches_consensus$votes_prop >= consensus_threshold, TRUE, FALSE)
matches_consensus$warning_disagreement <- ifelse(consensus_threshold > matches_consensus$votes_prop & matches_consensus$votes_prop >= warning_threshold, TRUE, FALSE)
entropy_threshold <- 1
entropy_calc <- function(x){
  -sum(x*log(x))
}
entropy <- matches_consensus %>% select(subject_id, votes_prop)
entropy <- aggregate(entropy, by = list(entropy$subject_id), FUN = entropy_calc)
entropy <- entropy %>% select(Group.1, votes_prop)
colnames(entropy) <- c("subject_id","entropy")
entropy$entropy_warning <- ifelse(entropy$entropy > entropy_threshold, TRUE, FALSE)
matches_consensus <- merge(matches_consensus, entropy, by = "subject_id", all.x = TRUE)

matches_consensus_accepted <- matches_consensus %>% filter(n_classifications > 10) %>% filter(classification_accept==TRUE)

matches_consensus_accepted$expert_match <- NA
for(i in 1:nrow(matches_consensus_accepted)){
  temp1 <- expert_sub %>% filter(subject_id == matches_consensus_accepted$subject_id[i])
  
  if(matches_consensus_accepted$species[i] %in% temp1$species){
    matches_consensus_accepted$expert_match[i] <- 1
  }else{
    matches_consensus_accepted$expert_match[i] <- 0
  }
}

# Generate confusion matrix for raw and pseudo-consensus classifications 
# Raw classifications
expert_sub$expert_species <- expert_sub$species

class_sub <- merge(volunteer_sub, expert_sub, by = c("subject_id","species"), all = TRUE)
class_sub2 <- merge(class_sub, expert_sub, by = "subject_id", all.y = TRUE)
class_sub <- class_sub2; rm(class_sub2)

class_sub <- class_sub %>% mutate(expert_species = coalesce(expert_species.x, expert_species.y))
class_sub$expert_species <- as.factor(class_sub$expert_species)

# Standardise factor levels across species columns
x <- as.data.frame(fct_unify(list(class_sub$expert_species, class_sub$species.x)))
colnames(x) <- c("A","B")
class_sub$expert_species <- x$A
class_sub$species.x <- x$B

# Generate confusion matrix
cmat <- confusionMatrix(data = class_sub$species.x,
                        reference = class_sub$expert_species)

cmat2 <- cmat$table
cmat3 <- as.data.frame(cmat2)

x <- as.factor(levels(class_sub$species.x))
y <- x
df <- expand.grid(x,y); rm(x,y)
colnames(df) <- c("volunteer","expert")

df2 <- merge(df, cmat2, by.x = c("volunteer","expert"), by.y = c("Prediction","Reference"))

slist <- as.data.frame(levels(class_sub$species.x))
colnames(slist) <- "species"
slist$Freq_tot <- NA

for(i in 1:nrow(slist)){
  slist$Freq_tot[i] <- sum(df2$Freq[df2$expert==slist$species[i]])
}

df3 <- merge(df2, slist, by.x = "expert", by.y = "species")

df3$Freq_std <- df3$Freq / df3$Freq_tot
df3$Freq_std[is.na(df3$Freq_std)] <- 0

p1 <- ggplot(df3, aes(volunteer, expert, fill= Freq_std)) + 
  geom_tile() +
  scale_fill_viridis(option = "inferno", direction = 1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position="none")

#heatmap(cmat2, Colv = NA, Rowv = NA, scale = "column")

# Pseudo-consensus classifications ####

# Can also make this for consensus classifications vs. expert - how often do they match?
matches_consensus_sub <- matches_consensus_accepted %>% select(subject_id,
                                                               species,
                                                               expert_match)

expert_sub2 <- expert_sub %>% select(subject_id,
                                     species,
                                     expert_match)

expert_sub2$expert_species <- expert_sub$species

class_sub <- merge(matches_consensus_sub, expert_sub2, by = c("subject_id","species"), all = TRUE)
class_sub2 <- merge(class_sub, expert_sub2, by = "subject_id", all.y = TRUE)
class_sub <- class_sub2; rm(class_sub2)

class_sub <- class_sub %>% mutate(expert_species = coalesce(expert_species.x, expert_species.y))
class_sub$expert_species <- as.factor(class_sub$expert_species)

# Standardise factor levels across species columns
x <- as.data.frame(fct_unify(list(class_sub$expert_species, class_sub$species.x)))
colnames(x) <- c("A","B")
class_sub$expert_species <- x$A
class_sub$species.x <- x$B

# Generate confusion matrix
cmat <- confusionMatrix(data = class_sub$species.x,
                        reference = class_sub$expert_species)

cmat2 <- cmat$table
cmat3 <- as.data.frame(cmat2)

x <- as.factor(levels(class_sub$species.x))
y <- x
df <- expand.grid(x,y); rm(x,y)
colnames(df) <- c("volunteer","expert")

df2 <- merge(df, cmat2, by.x = c("volunteer","expert"), by.y = c("Prediction","Reference"))

slist <- as.data.frame(levels(class_sub$species.x))
colnames(slist) <- "species"
slist$Freq_tot <- NA

for(i in 1:nrow(slist)){
  slist$Freq_tot[i] <- sum(df2$Freq[df2$expert==slist$species[i]])
}

df3 <- merge(df2, slist, by.x = "expert", by.y = "species")

df3$Freq_std <- df3$Freq / df3$Freq_tot
df3$Freq_std[is.na(df3$Freq_std)] <- 0

p2 <- ggplot(df3, aes(volunteer, expert, fill= Freq_std)) + 
  geom_tile() +
  scale_fill_viridis(option = "inferno", direction = 1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position="none")

# Both plots together
grid1 <- plot_grid(p2, p1, labels = c("A","B"))
