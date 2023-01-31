library(readxl)
library(ggroups)
library(itertools)

dane <- read_excel("E:/Studia/Magisterka/1_rok/zZofia/dane 2.xlsx")

# Obróbka danych do BLUP

vec <- c(dane$`nr os`, dane$`nr oj`, dane$`nr ma`)
new <- data.frame(ID=sort(unique(vec)), SIRE=0, DAM=0)

for (i in as.list(enumerate(new$ID))){
  if (i$value %in% dane$`nr os`) {
    new[i$index, 2] <- dane$`nr oj`[dane$`nr os` == i$value]
    new[i$index, 3] <- dane$`nr ma`[dane$`nr os` == i$value]
    #new[i$index, 4] <- dane$LOP[dane$`nr os` == i$value]
  }
}

# Macierz spokrewnień
library(AGHmatrix)

a <- Amatrix(new)

#Najcześciej wybierani ojcowie
b <- table(dane$`nr oj`)
b <- b[b > 30]
b

#Najcześciej wybierane matki
c <- table(dane$`nr ma`)
c <- c[c >= 15]
c
