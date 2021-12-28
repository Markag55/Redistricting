OHEnacted <- read.csv("~\\Gerrymander\\OH-Enacted.csv", header = TRUE) #Get Ohio Enacted Map
OHProp <- read.csv("~\\Gerrymander\\OH-Proportional.csv", header=TRUE) #Get Ohio Fair Map
ILEnacted <- read.csv("~\\Gerrymander\\IL-Enacted.csv", header=TRUE)   #Get Illinois Enacted Map
ILProp <- read.csv("~\\Gerrymander\\IL-Proportional.csv", header=TRUE) #Get Illinois Fair Map
COEnacted <- read.csv("~\\Gerrymander\\CO-Enacted.csv", header=TRUE)   #Get Colorado Enacted Map
COProp <- read.csv("~\\Gerrymander\\CO-Proportional.csv", header=TRUE) #Get Colorado Fair Map
MIEnacted <- read.csv("~\\Gerrymander\\MI-Enacted.csv", header=TRUE)   #Get Michigan Enacted Map
MIProp <- read.csv("~\\Gerrymander\\MI-Proportional.csv", header=TRUE) #Get Michigan Fair Map
Map_Score <- function(Map1, Map2, Size, Begin) {
  numOfCompetitive1 = 0
  numOfD1 = 0;
  numOfR1 = 0;
  numOfCompetitive2 = 0
  numOfD2 = 0
  numOfR2 = 0
  Competitvness1 <- Map1[2:(Size+1),Begin]-Map1[2:(Size+1),(Begin+1)] #Find the partisan advantage of each district
  Competitvness2 <- Map2[2:(Size+1),Begin]-Map2[2:(Size+1),(Begin+1)]
  #print(Map1[2:(Size+1),Begin:(Begin+1)])
  #print(Map2[2:(Size+1),Begin:(Begin+1)])
  #print(Competitvness1)
  #print(Competitvness2)
  for (i in Competitvness1[1:Size])
  {
    if (abs(i) <= 0.1)
    {
      numOfCompetitive1 = numOfCompetitive1 + 1 #Add to the number of competitive districts in Map1
    }
    else if(i > 0.1)
    {
      numOfD1 = numOfD1 + 1 #Add to the number of Democratic districts in Map1
    }
    else if(i < 0.1)
    {
      numOfR1 = numOfR1 +1 #Add to the number of Republican districts in Map1
    }
  }
  for (j in Competitvness2[1:Size])
  {
    if (abs(j) <= 0.1)
    {
      numOfCompetitive2 = numOfCompetitive2 + 1 #Add to the number of competitive districts in Map2
    }
    else if(j > 0.1)
    {
      numOfD2 = numOfD2 + 1 #Add to the number of Democratic districts in Map2
    }
    else if(j < 0.1)
    {
      numOfR2 = numOfR2 +1 #Add to the number of Republican districts in Map2
    }
  }
  #print(numOfCompetitive1)
  #print(numOfCompetitive2)
  CompetitveDifferance <- abs(numOfCompetitive1-numOfCompetitive2) #Get the difference of competitive districts between Map1 and Map2
  DifferenceD <- abs(numOfD1-numOfD2) #Get the difference of Democratic districts between Map1 and Map2
  DifferenceR <- abs(numOfR1-numOfR2) #Get the difference of Republican districts between Map1 and Map2
  return((CompetitveDifferance+DifferenceD+DifferenceR)/Size) #Add the differences between competitive, Democrat, and Republican districts and divide by the number of districts to get the fairness score
  
}
OHScore <- Map_Score(OHEnacted, OHProp, 15, 4)
ILScore <- Map_Score(ILEnacted, ILProp, 17, 3)
COScore <- Map_Score(COEnacted, COProp, 8, 4)
MIScore <- Map_Score(MIEnacted, MIProp, 13, 3)
print(paste("Ohios Score:", OHScore))
print(paste("Illinois Score:", ILScore))
print(paste("Colorado Score:", COScore))
print(paste("Michigan Score", MIScore))
print(paste("The fairest map has a score of:", min(OHScore, ILScore, COScore, MIScore)))

