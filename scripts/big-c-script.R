library(SemNetDictionaries)
library(SemNetCleaner)
library(SemNeT)
library(readxl)
library(data.table)
BigCAll <- read_excel("data/raw/bigCAll.xlsx")

BigCClean <- textcleaner(data = BigCAll[,-2], miss = 99,
                         partBY = "row", dictionary = "animals")

write.csv(BigCClean$responses$clean, file = "data/clean/BigCClean.csv")

gsemoup <- BigCAll$group

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

write.csv(BigC_Bootstrap$measures, file = "networks/bootstrapping_measures.csv", row.names = TRUE)
fwrite(BigC_Bootstrap$bootstrapTest$ASPL, file = "networks/bootstrapTest_ASPL.csv", row.names = TRUE)
fwrite(BigC_Bootstrap$bootstrapTest$CC, file = "networks/bootstrapTest_CC.csv", row.names = TRUE)
fwrite(BigC_Bootstrap$bootstrapTest$Q, file = "networks/bootstrapTest_Q.csv", row.names = TRUE)
