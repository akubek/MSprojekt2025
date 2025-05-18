#Opracować test permutacyjny do sprawdzenia polecenia opisanego w punkcie 6.

polec7 <- function(dane1,dane2) {
  alpha = 0.05
  
  n1 = length(dane1)
  n2 = length(dane2)
  
  liczba_perm = 10000
  
  sr1 = mean(dane1)
  sr2 = mean(dane2)

  #statystyka testowa
  R0 = sr1 - sr2
  
  zlaczenie = c(dane1,dane2)
  n = n1+n2
  
  roznice = numeric(liczba_perm)
  
  #powtorzenie permutacji
  for(i in 1:liczba_perm) {
    perm = sample(zlaczenie,n)
    g1 = perm[1:n1]
    g2 = perm[(n1+1):n]
    roznice[i] = mean(g1) - mean(g2) #roznice miedzy srednimi
  }
  
  p = mean(abs(roznice) >= abs(R0)) #wartosc p
  
  cat("Obserwowana różnica średnich:", R0, "\n")
  cat("P-wartość jednostronna:", p, "\n")
  if (p < alpha) {
    cat("Odrzucamy H0: istnieje istotna różnica między średnimi (p =", p, " < ", alpha, ")\n")
  } else {
    cat("Nie odrzucamy H0: brak istotnej różnicy między średnimi (p =", p, " >= ", alpha, ")\n")
  }
}