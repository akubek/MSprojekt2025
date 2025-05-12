# POLECENIE 3
# Sprawdzić, czy wytrzymałości na rozciąganie mają rozkład normalny 
# (test zgodności Kołmogorowa-Lillieforsa, współczynnik ufności 0,95).

polec3 <- function(dane1,dane2) {
  dlug1 = length(dane1)
  dlug2 = length(dane2)
  #krok 1 - średnia arytmetyczna
  sr1 = mean(dane1)
  sr2 = mean(dane2)
  #krok 2 - odcyhlenie standardowe - nieobciążone
  odch1 = sd(dane1)
  odch2 = sd(dane2)
  #krok 3 - sortowanie
  pos1 = sort(dane1)
  pos2 = sort(dane2)
  
  
  #krok 4 - dystrybuanta empiryczna
  wart1 = (1:dlug1)/dlug1
  wart2 = (1:dlug2)/dlug2
  
  #krok 5 - dystrybuanta rozkładu normalnego
  norm1 = pnorm(pos1, mean = sr1, sd = odch1)
  norm2 = pnorm(pos2, mean = sr2, sd = odch2)
  
  #porównanie wykresów dystrybuanty empirycznej i dystrybuanty dla rozkładu normalnego
  png("polecenie_3-1.png",width=800,height=800)
  plot(pos1,wart1,type="s", main="Porównanie dystrybuanty rozkładu normalnego i dystrybuanty empirycznej dla walcowni 1",
       xlab="wartości", ylab="F(x)")
  lines(pos1, norm1, col="red")
  dev.off()
  png("polecenie_3-2.png",width=800,height=800)
  plot(pos2,wart2,type="s", main="Porównanie dystrybuanty rozkładu normalnego i dystrybuanty empirycznej dla walcowni 2",
       xlab="wartości", ylab="F(x)")
  lines(pos2, norm2, col="red")
  dev.off()
  
  #krok 6 - wartosc statystyki testowej
  d1 = abs(wart1[1]-norm1[1])
  d2 = abs(wart2[1]-norm2[1])
  for(i in 2:dlug1) {
    dn = max(abs(wart1[i-1]-norm1[i]), abs(wart1[i]-norm1[i]))
    if(dn > d1) {
      d1 = dn
    }
  }
  
  for(i in 2:dlug2) {
    dn = max(abs(wart2[i-1]-norm2[i]), abs(wart2[i]-norm2[i]))
    if(dn > d2) {
      d2 = dn
    }
  }
  
  cat("Wartość statystyki testowej dla pierwszej walcowni: d1 = ", d1, ", dla drugiej walcowni: d2 =", d2 ,"\n")
  
  #krok 7 - zbiór krytyczny
  wart_kryt1 = 0.1245
  wart_kryt2 = 0.1270
  
  cat("Zbiór krytyczny dla walcowni pierwszej (n=50,alpha=0.05) to K1_0 = <",wart_kryt1,",1>\n")
  if(d1 < wart_kryt1) {
    cat("Statystyka testowa d1 nie należy do zbioru krytycznego K1_0, więc nie ma podstaw do odrzucenia hipotezy zerowej dla pierwszej walcowni.\n")
  } else {
    cat("Statystyka testowa d1 należy do zbioru krytycznego K1_0, więc odrzucamy hipotezę zerową na korzyść hipotezy alternatywnej.\n")
  }
  
  cat("Zbiór krytyczny dla walcowni drugiej (n=48,alpha=0.05) to K2_0 = <",wart_kryt2,",1>\n")
  if(d2 < wart_kryt2) {
    cat("Statystyka testowa d2 nie należy do zbioru krytycznego K2_0, więc nie ma podstaw do odrzucenia hipotezy zerowej dla drugiej walcowni.\n")
  } else {
  cat("Statystyka testowa d2 należy do zbioru krytycznego K2_0, więc odrzucamy hipotezę zerową na korzyść hipotezy alternatywnej.\n")
  }
}

