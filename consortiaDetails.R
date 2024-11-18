sexGroups <- c("Female", "Male")
consortiumName <- "Research Consortium" 
regions <- c("A", "B")
fullRegionNames <- c("Site A", "Site B")
totalProgsPerRegion <- c(5, 4)

  minyear <- 2010
  maxyear <- 2023
  server_name <- "research"
  timeVar <- "Year"
  groupVar <- "Site"
  consortiaWebsite <- "https://www.research.org" # fake example
  websiteName <- "Research Consortium"

timeVarSym <- rlang::sym(timeVar)

groupVarSym <- rlang::sym(groupVar)