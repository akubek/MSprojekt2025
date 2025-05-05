# POLECENIE 2
# Dokonać analizy wytrzymałości na rozciąganie, wyznaczając miary przeciętne,
# zróżnicowania, asymetrii i koncentracji.
# Opracować histogramy rozkładów empirycznych. Miary wyznaczyć dwoma sposobami:
# a) na podstawie szeregu szczegółowego
# b) na podstawie szeregu rozdzielczego.

polec2 <- function(dane1,dane2) {
  #szreg rozdzielczy
  #ilość grup - pierwiastek z liczby obserwacji
  b1 = as.integer(sqrt(length(dane1)))
  b2 = as.integer(sqrt(length(dane2)))
  
  #histogram - hist()
  hist1 = hist(y1,breaks=b1)
  hist2 = hist(y2,breaks=b2)
  
  # Miary przecietne - położenia:
  # 1) średnia
  # a) dla sz. szczegółowego
  sr_szcz1 = mean(dane1)
  sr_szcz2 = mean(dane2)
  
  # b) dla sz. rozdzielczego
  sr_roz1=0
  for(i in 1:length(hist1)) {
    sr_roz1 = sr_roz1 + hist1$counts[i] * hist1$mids[i]
  }
  sr_roz1 = sr_roz1/length(y1)
  sr_roz2 = 0
  for(i in 1:length(hist2)) {
    sr_roz2 = sr_roz2 + hist2$counts[i] * hist2$mids[i]
  }
  sr_roz2 = sr_roz2/length(y2)
  
  # 2) mediana
  # a) dla sz. szczegółowego
  med_szcz1 = median(y1)
  med_szcz2 = median(y2)
  
  # b) dla sz. rozdzielczego
  
  # - moda - tylko szereg rozdzielczy
  #which.max() - moda
  moda1 = hist1$mids[which.max(hist1$counts)]
  moda2 = hist2$mids[which.max(hist2$counts)]
  
  # - kwartyle
  
  # Miary zróżnicowania - rozproszenia:
  # - odchylenie standardowe (obc, nieobc)
  # - wariancja(obc, nieobc)
  # - odchylenie przeciętne
  # - odchylenie przeciętne od mediany
  # - odchylenie ćwiartkowe Q
  # - współczynnik zmienności w%v
  # - pozycyjny współczynnik zmienności w%v_q
  
  # Miary asymetrii i koncentracji:
  # - skośność a_s
  # - kurtoza krt
  # - eksces g_2
  
  #cumsum() - suma skumulowana
  #findInterval()
  
  sprintf("Srednia dla szeregu szczegolowego, sr1 = %f, sr2 = %f",sr_szcz1, sr_szcz2)
  sprintf("Srednia dla szeregu rozdzielczego, sr1 = %f, sr2 = %f",sr_roz1, sr_roz2)
  
  sprintf("Mediana dla szeregu szczegolowego, mediana1 = %f, mediana2 = %f",mediana1, mediana2)
  sprintf("Mediana dla szeregu rozdzielczego, sr1 = %f, sr2 = %f",sr_roz1, sr_roz2)
  
  sprintf("Moda dla szeregu rozdzielczego, moda1 = %f, moda2 = %f",moda1, moda2)
  
}

