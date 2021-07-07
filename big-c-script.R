library(SemNetDictionaries)
library(SemNetCleaner)
library(SemNeT)
library(readxl)

BigCAll <- read_excel("data/bigCAll.xlsx")

BigCClean <- textcleaner(data = BigCAll[,-c(1:2)], miss = 99,
                         partBY = "row", dictionary = "animals")

artists <- BigCClean$responses$clean[which(group == "artists"),]
scientists <- BigCClean$responses$clean[which(group == "scientists"),]
controls <- BigCClean$responses$clean[which(group == "controls"),]

write.csv(artists, file = "data/clean/artists_cleaned_responses.csv", row.names = TRUE)
write.csv(scientists, file = "data/clean/scientists_cleaned_responses.csv", row.names = TRUE)
write.csv(controls, file = "data/clean/controls_cleaned_responses.csv", row.names = TRUE)

artistNet <- TMFG_BigC$network$artists
scienceNet <- TMFG_BigC$network$scientists
controlNet <- TMFG_BigC$network$controls

write.csv(artistNet, file = "networks/artist_network.csv", row.names = TRUE)
write.csv(scienceNet, file = "networks/scientist_network.csv", row.names = TRUE)
write.csv(controlNet, file = "networks/control_network.csv", row.names = TRUE)