# POLECENIE 1
# Zaprezentować dane wykorzystując wykresy pudełkowe. Przygotować dwa typy wykresów prezentujących: 
# 1) medianę i kwartale
# 2) wartość oczekiwaną i odchylenie standardowe.

polec1 <- function(dane1,dane2) {#tworzenie funkcji polec1, ktora przyjmuje 2 argumenty
  lista_danych = list(dane1,dane2)
  
  #wykres pudełkowy 2 zestawów danych
  #mediana, kwartale
  png("polecenie_1-1.png",width=900,height=850) #zapis jako zdjecie
  boxplot(lista_danych, names = c("Walcownia 1", "Walcownia 2"),
           main = "Wykres pudełkowy - Mediana i Kwartyle",
           ylab = "Wartości [MPa]",
           col = c("orange", "pink"))
  abline(h = seq(400, 850, by = 25), col = "lightgrey", lty = 2, lwd = 1)
  dev.off()
  
  
  #średnia i odchylenie standardowe
  #wartość oczekiwana rozkładu normalnego - średnia
  sr1=mean(dane1)
  sr2=mean(dane2)
  odch1=sd(dane1)
  odch2=sd(dane2)
  
  #wykres pudełkowy
  #wekt 5el - i boxplot
  # 1 - min
  # 2 - (sr - odchylenie)
  # 3 - sr
  # 4 - (sr + odchylenie)
  # 5 - max
  w1 = c(min(y1),sr1-odch1,sr1,sr1+odch1,max(y1))
  w2 = c(min(y2),sr2-odch2,sr2,sr2+odch2,max(y2))
  wartosci=list(w1,w2)
  png("polecenie_1-2.png",width=900,height=850)
  boxplot(wartosci,names = c("Walcownia 1", "Walcownia 2"),
          main = "Wykres pudełkowy - Średnia i odchylenie standardowe",
          ylab = "Wartości [MPa]",
          col = c("orange", "pink"))
  abline(h = seq(400, 850, by = 25), col = "lightgrey", lty = 2, lwd = 1)
  dev.off()
  
  
  # wykres dla polecenia 2 - niewykorzystane
  # srednie=c(sr1,sr2)
  # odchylenia=c(odch1,odch2)
  # 
  # plot(1:2,srednie,
  #      main="Średnia i Odchylenie Standardowe",
  #      xlab="Pliki",
  #      ylab="Wartosci",
  #      xaxt="n",
  #      ylim=c(min(srednie-odchylenia),max(srednie+odchylenia)))
  # arrows(x0 = 1:2,
  #        y0 = srednie-odchylenia,
  #        x1 = 1:2,
  #        y1 = srednie+odchylenia,
  #        angle = 90, code = 3, length = 0.1, col = "red")
  # axis(1,at=1:2,labels=c("Plik 1", "Plik 2"))
}



