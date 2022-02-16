#Install packages
#this takes about 30 minutes so be prepared
install.packages(c("SemNetDictionaries",
                    "SemNetCleaner",
                    "SemNeT"),
                 dependencies = c("Imports",
                                  "Suggests"))

# Additional packages for Shiny GUI
install.packages(c("shiny",
                    "shinyjs",
                    "shinyalert",
                    "shinyMatrix",
                    "shinyBS"))

#Load packages
library(SemNetCleaner)
library(SemNeT)

#Raw VF data included in SemNetCleaner
data("open.animals")
head("open.animals")

dictionaries()
load.dictionaries("animals")
load.monikers("animals")

#load.dictionaries("f")
#load.dictionaries("general")
#load.dictionaries("fruits", "vegetables")
#append.dictionary("list_of_words", "name", "location")

#don't separate your data into groups yet or you'll have to do this twice
#starts spell-check and autocorrect
#and then will ask you to make some decisions
clean <- textcleaner(data = open.animals[, -c(1:2)], #removing irrelevant rows; leave subject ID
                     miss = 99, partBY = "row", 
                     dictionary = "animals")

#output is stored in a list object (clean)
#nested objects:
clean$behavioral #perseverations, intrusions, no. appropriate responses

#split data into groups
#this one's on "openness to experience"
group <- ifelse(open.animals$Group == 1, "Low", "High")

#there's a nice GUI to estimate networks
SemNeTShiny()
