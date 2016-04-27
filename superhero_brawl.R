#Superhero Brawl

#Randomly builds superheroes, then pits them in battles against each other!

#set your working directory
#setwd("/superhero_brawl")

#set.seed(333333)

class <- read.csv("class.csv",stringsAsFactors = FALSE)
powers <- read.csv("superpower.data.csv", stringsAsFactors = FALSE)

#Randomly pick one class and two powers
class_index1 <- sample(1:length(class$Class),1)
powers_index1 <- sample(1:length(powers$Index),2, replace = FALSE)
class_index2 <- sample(1:length(class$Class),1)
powers_index2 <- sample(1:length(powers$Index),2, replace = FALSE)

#Create hero datatables with the above information
hero1 <- rbind(powers[powers_index1[1],], powers[powers_index1[2],])
hero2 <- rbind(powers[powers_index2[1],], powers[powers_index2[2],])

#pick a random number 0 or 1. This determines whether Name1 or Name2 is chosen.
#Name is in the form of <Name1 OR Name 2><Class suffix>
random1 <- sample(0:1,1)
name1 <- paste0(powers[powers_index1[1],13+random1], class$Class.Name[class_index1])
random2 <- sample(0:1,1)
name2 <- paste0(powers[powers_index2[1],13+random2], class$Class.Name[class_index2])

#Build the attack library using the stronger of each hero's two stats
Attacks1 <- c(1,1,1,1,1)
Attacks2 <- c(1,1,1,1,1)

for (i in 1:5){
    if (hero1[2, i+2] > hero1[1, i+2]){Attacks1[i] <- 2}
}

for (i in 1:5){
    if (hero2[2, i+2] > hero2[1, i+2]){Attacks2[i] <- 2}
}

#Introducing the combatants
script <- ""

script[1] <- paste0("The first hero is ", name1, ", the mighty ", class$Class[class_index1], " with the powers of ", 
       hero1$Superpower[1], " and ", hero1$Superpower[2], "!")
script[2] <- paste0("The second hero is ", name2, ", the mighty ", class$Class[class_index2], " with the powers of ", 
       hero2$Superpower[1], " and ", hero2$Superpower[2], "!")

#Fight! Attacks1/2 are indices for which of the two powers to use. The powers are in the hero1/2 variables.
script[3] <- ""
m <- 4
n <- 3
score <- c(0,0)
for (i in Attacks1){
    script[m] <- paste0(name1, " ", hero1[i,n+5])
    score[1] <- score[1] + hero1[i,n]
    n <- n+1
    m <- m+2
}

m <- 5
n <- 3

for (i in Attacks2){
    script[m] <- paste0(name2, " ", hero2[i,n+5])
    score[2] <- score[2] + hero2[i,n]
    n <- n+1
    m <- m+2
}

script[14] <- ""

#Aftermath
if (score[1]==score[2]){
    #Tie. Flip a coin.
    if (sample(0:1,1)==0){
        script[15] <- paste0(name1, " ", class[class_index1, 2])
        script[16] <- paste0(name2, " ", class[class_index2, 3])
    } else {
        script[15] <- paste0(name1, " ", class[class_index1, 3])
        script[16] <- paste0(name2, " ", class[class_index2, 2])       
    }
} else if (score[1]>score[2]) {
    script[15] <- paste0(name1, " ", class[class_index1, 2])
    script[16] <- paste0(name2, " ", class[class_index2, 3])
} else {
    script[15] <- paste0(name1, " ", class[class_index1, 3])
    script[16] <- paste0(name2, " ", class[class_index2, 2]) 
}

script