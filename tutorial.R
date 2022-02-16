#Installpackages
install.packages(c(“SemNetDictionaries”,
                    “SemNetCleaner”,
                    “SemNeT”),dependencies = c(“Imports”,“Suggests”))
# Additional packages for Shiny GUI
install.packages(c(“shiny”,
                    “shinyjs”,
                    “shinyalert”,
                    “shinyMatrix”,
                    “shinyBS”))