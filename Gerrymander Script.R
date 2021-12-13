OHEnacted <- read.csv("~\\Gerrymander\\OH-Enacted.csv", header = TRUE)
OHProp <- read.csv("~\\Gerrymander\\OH-Proportional.csv", header=TRUE)
ILEnacted <- read.csv("~\\Gerrymander\\IL-Enacted.csv", header=TRUE)
ILProp <- read.csv("~\\Gerrymander\\IL-Proportional.csv", header=TRUE)
COEnacted <- read.csv("~\\Gerrymander\\CO-Enacted.csv", header=TRUE)
COProp <- read.csv("~\\Gerrymander\\CO-Proportional.csv", header=TRUE)
Map_Score <- function(Map1, Map2, Size, Begin) {
  numOfCompetitive1 = 0
  numOfD1 = 0;
  numOfR1 = 0;
  numOfCompetitive2 = 0
  numOfD2 = 0
  numOfR2 = 0
  Competitvness1 <- Map1[2:(Size+1),Begin]-Map1[2:(Size+1),(Begin+1)]
  Competitvness2 <- Map2[2:(Size+1),Begin]-Map2[2:(Size+1),(Begin+1)]
  #print(Map1[2:(Size+1),Begin:(Begin+1)])
  #print(Map2[2:(Size+1),Begin:(Begin+1)])
  #print(Competitvness1)
  #print(Competitvness2)
  for (i in Competitvness1[1:Size])
  {
    if (abs(i) <= 0.1)
    {
      numOfCompetitive1 = numOfCompetitive1 + 1
    }
    else if(i > 0.1)
    {
      numOfD1 = numOfD1 + 1
    }
    else if(i < 0.1)
    {
      numOfR1 = numOfR1 +1
    }
  }
  for (j in Competitvness2[1:Size])
  {
    if (abs(j) <= 0.1)
    {
      numOfCompetitive2 = numOfCompetitive2 + 1
    }
    else if(j > 0.1)
    {
      numOfD2 = numOfD2 + 1
    }
    else if(j < 0.1)
    {
      numOfR2 = numOfR2 +1
    }
  }
  #print(numOfCompetitive1)
  #print(numOfCompetitive2)
  CompetitveDifferance <- abs(numOfCompetitive1-numOfCompetitive2)
  DifferenceD <- abs(numOfD1-numOfD2)
  DifferenceR <- abs(numOfR1-numOfR2)
  return((CompetitveDifferance+DifferenceD+DifferenceR)/Size)
  
}
OHScore <- Map_Score(OHEnacted, OHProp, 15, 4)
ILScore <- Map_Score(ILEnacted, ILProp, 17, 3)
COScore <- Map_Score(COEnacted, COProp, 8, 4)
print(paste("Ohios Score:", OHScore))
print(paste("Illinois Score:", ILScore))
print(paste("Colorado Score:", COScore))
print(paste("The fairest map has a score of:",min(OHScore, ILScore, COScore)))

