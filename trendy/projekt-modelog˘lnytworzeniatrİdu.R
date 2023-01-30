# kk - kolor okrywy w³osowej
unique(daneMOGZ$`c7(GA)`)

# kk1 - kolor okrywy w³osowej we wszytskich stadach
daneMOGZ$kk1 <- 0
daneMOGZ$rok = daneMOGZ$rok+1900
for (x in unique(daneMOGZ$rok)){
  print(x)
  daneMOGZ$kk1[daneMOGZ$rok == x] = round(mean(daneMOGZ$`c7(GA)`[daneMOGZ$rok == x]), 2)
}
unique((daneMOGZ$kk1))

# kk2 - kolor okrywy w³osowej ze wzglêdu na stado
daneMOGZ$kk2 <- 0
for (x in unique(daneMOGZ$rok)){
  daneMOGZ$kk2[daneMOGZ$rok == x & daneMOGZ$ferma == 1] <- round(mean(daneMOGZ$`c7(GA)`[daneMOGZ$rok == x & daneMOGZ$ferma == 1]), 2)
  daneMOGZ$kk2[daneMOGZ$rok == x & daneMOGZ$ferma == 2] <- round(mean(daneMOGZ$`c7(GA)`[daneMOGZ$rok == x & daneMOGZ$ferma == 2]), 2)
  daneMOGZ$kk2[daneMOGZ$rok == x & daneMOGZ$ferma == 3] <- round(mean(daneMOGZ$`c7(GA)`[daneMOGZ$rok == x & daneMOGZ$ferma == 3]), 2)
}

unique(daneMOGZ$kk2)
daneMOGZ$kk2[daneMOGZ$rok == 1993 & daneMOGZ$ferma == 3] <- NaN
daneMOGZ$rok[daneMOGZ$rok == 1993] <- NaN

#wizualizacja

plot(daneMOGZ$rok, 
     daneMOGZ$kk1,
     type='o',
     pch =16,
     lty=3,
     col='pink',
     lwd=5,
     ylim = c(0,4),
     main="Trend genetyczny",
     col.main='black',
     xlab="Rok",
     ylab="Œrednia ocena wygl¹du ogólnego"
)
lines(daneMOGZ$rok[daneMOGZ$ferma == 1], 
      daneMOGZ$kk2[daneMOGZ$ferma == 1],
      type='o',
      pch =16,
      col='blue',
      lty=1,
      lwd=2)
lines(daneMOGZ$rok[daneMOGZ$ferma == 2], 
      daneMOGZ$kk2[daneMOGZ$ferma == 2],
      type='o',
      pch =16,
      lty=1,
      col='green',
      lwd=2)
lines(daneMOGZ$rok[daneMOGZ$ferma == 3], 
      daneMOGZ$kk2[daneMOGZ$ferma == 3],
      type='o',
      lty=1,
      pch =16,
      col='red',
      lwd=2)
grid()
legend("bottomleft", 
       c('wszytskie stada', '1 stado', '2 stado', '3 stado'), 
       col=c("pink", 'blue', 'green', 'red'), 
       lty = c(3,1,1,1), cex=0.8)


