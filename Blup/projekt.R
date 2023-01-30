library(readxl)
library(ggroups)
library(ggplot2)
library(itertools)
library(pedigree)

dane <- read_excel("E:/Pobrane/dane 2.xlsx")

# Obróbka danych do BLUP

new1 <- data.frame(ID=dane$`nr os`, SIRE=dane$`nr oj`, DAM=dane$`nr ma`, stado=dane$ferma, LOP=dane$LOP)
new2 <- data.frame(ID=dane$`nr os`, SIRE=dane$`nr oj`, DAM=dane$`nr ma`, stado=dane$ferma, CT=dane$`c2(CT)`)
new3 <- data.frame(ID=dane$`nr os`, SIRE=dane$`nr oj`, DAM=dane$`nr ma`, stado=dane$ferma, GA=dane$`c7(GA)`)

# Potrzebna jest odziedziczalność od Natalii
h1 <- 0.71
h2 <- 0.2
h3 <- 0.2


# Funkcja która oblicza BLUP
sol1 <- blup(LOP~1, ped = new1, alpha = ((1-h1)/h1))
sol2 <- blup(CT~1, ped = new2, alpha = ((1-h2)/h2))
sol3 <- blup(GA~1, ped = new3, alpha = ((1-h3)/h3))

#Funkcja do zmiany danych na dane potrzebne do wykresu, o którym pisałem
dane_do_wykresu <- function(wynik_BLUP, data){
  m <- wynik_BLUP@x[1] #średnia \mu
  result <- as.matrix(wynik_BLUP) #odchylenia
  result <- data.frame(data$ID ,m + result[2:nrow(wynik_BLUP)])
  colnames(result) <- c('ID', 'BLUP')
  result <- result[result$ID %in% dane$`nr os`,]
  result$rok <- dane$rok
  result$ferma <- dane$ferma
  result$MEAN <- m
  result$ID_wykres <- 1:(nrow(wynik_BLUP)-1)
  
  return(result)
}

# Gotowe dane do wykresu ze średnią wartością hodowlaną i odchyleniami
dane1 <- dane_do_wykresu(sol1, new1)

#Wykres
ggplot(data = dane1[1:nrow(dane1) %% 50 == 1, ])+
  geom_line(aes(x=ID_wykres, y=MEAN, color = 'MEAN'))+
  geom_point(aes(x=ID_wykres, y=BLUP, color = 'BLUP'))+
  geom_line(aes(x=ID_wykres, y=BLUP), color = 'lightblue')+
  scale_color_manual(name = "", values = c("MEAN" = "#af0b1e", 'BLUP' = '#af01be'), 
                     guide = guide_legend(override.aes = list(linetype = c(0, 1), shape = c(16, NA))))+
  theme(legend.position = 'right')+
  ggtitle('Łączna Ocena Pokroju', subtitle = 'Różowe punkty są sumą wyniku BLUP i średniej z BLUP. h^2 = 0.71')+
  theme(plot.title = element_text(face = 'bold', size = 16), plot.subtitle = element_text(size = 10))+
  ylab('LOP')+
  xlab('')

# Gotowe dane do wykresu ze oceną koloru okrywy włosowej
dane2 <- dane_do_wykresu(sol2, new2)

#Wykres
ggplot(data = dane2[1:nrow(dane2) %% 50 == 1, ])+
  geom_line(aes(x=ID_wykres, y=MEAN, color = 'MEAN'), size = 1)+
  geom_point(aes(x=ID_wykres, y=BLUP, color = 'BLUP'))+
  geom_line(aes(x=ID_wykres, y=BLUP), color = 'lightblue')+
  scale_color_manual(name = "", values = c("MEAN" = "#af0b1e", 'BLUP' = '#af01be'), 
                     guide = guide_legend(override.aes = list(linetype = c(0, 1), shape = c(16, NA))))+
  theme(legend.position = 'right')+
  ggtitle('Ocena Koloru Okrywy Włosowej Lisa', subtitle = 'Różowe punkty są sumą wyniku BLUP i średniej z BLUP. h^2 = 0.2')+
  theme(plot.title = element_text(face = 'bold', size = 16), plot.subtitle = element_text(size = 10))+
  ylab('CT')+
  xlab('')

# Gotowe dane do wykresu ze oceną ogólną lisa
dane3 <- dane_do_wykresu(sol3, new3)

#Wykres
ggplot(data = dane3[1:nrow(dane3) %% 50 == 1, ])+
  geom_line(aes(x=ID_wykres, y=MEAN, color = 'MEAN'), size = 1)+
  geom_point(aes(x=ID_wykres, y=BLUP, color = 'BLUP'))+
  geom_line(aes(x=ID_wykres, y=BLUP), color = 'lightblue')+
  scale_color_manual(name = "", values = c("MEAN" = "#af0b1e", 'BLUP' = '#af01be'), 
                     guide = guide_legend(override.aes = list(linetype = c(0, 1), shape = c(16, NA))))+
  theme(legend.position = 'right')+
  ggtitle('Ogólna Ocena Wyglądu Lisa', subtitle = 'Różowe punkty są sumą wyniku BLUP i średniej z BLUP. h^2 = 0.2')+
  theme(plot.title = element_text(face = 'bold', size = 16), plot.subtitle = element_text(size = 10))+
  ylab('GA')+
  xlab('')


