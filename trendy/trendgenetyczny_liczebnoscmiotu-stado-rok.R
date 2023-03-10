
# x = rok - daneMOGZ$rok
# y1 = liczebno?? miotu - daneMOGZ$liur
# y2 = liczebno?? miotu ze wzgl?du na stado - daneMOGZ$y2[daneMOGZ$rok == x & daneMOGZ$ferma == 1/2/3 ]

# spradzenie danych
#class(daneMOGZ$rok)
#class(daneMOGZ$liur)
#class(daneMOGZ$ferma)
#unique(daneMOGZ$rok)
#unique(daneMOGZ$liur)
#n = length(unique(daneMOGZ$rok))
#table( daneMOGZ$rok, daneMOGZ$liur )
#table( daneMOGZ$ferma, daneMOGZ$liur )


# y1 - ?rednia liczebno?? miotu we wszytskich stadach
daneMOGZ$y1 <- 0
for (x in unique(daneMOGZ$rok)){
  daneMOGZ$y1[daneMOGZ$rok == x] = round(mean(daneMOGZ$liur[daneMOGZ$rok == x]), 2)
}

#table( daneMOGZ$rok, daneMOGZ$y1 )

# y2 - ?rednia liczebno?? miotu ze wzgl?du na stado
daneMOGZ$y2 <- 0
for (x in unique(daneMOGZ$rok)){
  daneMOGZ$y2[daneMOGZ$rok == x & daneMOGZ$ferma == 1] <- round(mean(daneMOGZ$liur[daneMOGZ$rok == x & daneMOGZ$ferma == 1]), 2)
  daneMOGZ$y2[daneMOGZ$rok == x & daneMOGZ$ferma == 2] <- round(mean(daneMOGZ$liur[daneMOGZ$rok == x & daneMOGZ$ferma == 2]), 2)
  daneMOGZ$y2[daneMOGZ$rok == x & daneMOGZ$ferma == 3] <- round(mean(daneMOGZ$liur[daneMOGZ$rok == x & daneMOGZ$ferma == 3]), 2)
  }

daneMOGZ$rok <- daneMOGZ$rok + 1900  #!!! zrobi? tylko przy pierwszym runie

#table( daneMOGZ$rok, daneMOGZ$ferma)
daneMOGZ$y2[daneMOGZ$rok == 1993 & daneMOGZ$ferma == 3] <- NaN # w roku 93 nie badano innych stad
daneMOGZ$rok[daneMOGZ$rok == 1993] <- NaN

#WIZUALIZACJA



# zwyk?y wykres
plot(daneMOGZ$rok, 
     daneMOGZ$y1,
     type='o',
     pch =16,
     lt=3,
     col='pink',
     lwd=5,
     ylim = c(1,6),
     main="Trend genetyczny",
     col.main='black',
     xlab="Rok",
     ylab="?rednia liczebno?? miotu"
     )
lines(daneMOGZ$rok[daneMOGZ$ferma == 1], 
     daneMOGZ$y2[daneMOGZ$ferma == 1],
     type='o',
     pch =16,
     lt=1,
     col='blue',
     lwd=2)
lines(daneMOGZ$rok[daneMOGZ$ferma == 2], 
      daneMOGZ$y2[daneMOGZ$ferma == 2],
      type='o',
      pch =16,
      lt=1,
      col='green',
      lwd=2)
lines(daneMOGZ$rok[daneMOGZ$ferma == 3], 
      daneMOGZ$y2[daneMOGZ$ferma == 3],
      type='o',
      pch =16,
      lt=1,
      col='red',
      lwd=2)
grid()
legend("bottomleft", 
       c('Wszytskie stada', '1 stado', '2 stado', '3 stado'), 
       col=c("pink", 'blue', 'green', 'red'), 
       lty = c(3,1,1,1), cex=0.8)





