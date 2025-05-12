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
  sr_roz1 = sum(hist1$counts * hist1$mids)/length(y1)
  sr_roz2 = sum(hist2$counts * hist2$mids)/length(y2)
  
  # 2) mediana
  # a) dla sz. szczegółowego
  med_szcz1 = median(y1)
  med_szcz2 = median(y2)
  
  # b) dla sz. rozdzielczego
  dlugosc1 = length(dane1)
  dlugosc2 = length(dane2)
  
  polowa1 = floor(dlugosc1/2)
  polowa2 = floor(dlugosc2/2)
  
  csum1 = cumsum(hist1$counts)
  csum2 = cumsum(hist2$counts)
  
  indx_med1 = tail(which(csum1 <= polowa1),n=1)
  indx_med2 = tail(which(csum2 <= polowa2),n=1)
  
  med_rozdz1 = hist1$mids[indx_med1]
  med_rozdz2 = hist2$mids[indx_med2]
  
  #jeżeli ilość elementów w szeregu jest parzysta to mediana to średnia 2 liczb
  if(dlugosc1 %% 2 == 0) {
    #2ga liczba do wyliczenia mediany
    mm = tail(which(csum1 <= polowa1+1),n=1)
    med_rozdz1 = (med_rozdz1+hist1$mids[mm])/2
  }
  
  if(dlugosc2 %% 2 == 0) {
    mm = tail(which(csum2 <= polowa2+1),n=1)
    med_rozdz2 = (med_rozdz2+hist2$mids[mm])/2
  }
  
  # 3) moda - tylko szereg rozdzielczy
  #which.max() - moda
  moda1 = hist1$mids[which.max(hist1$counts)]
  moda2 = hist2$mids[which.max(hist2$counts)]
  
  # 4) kwartyle (1 i 3, 2 to mediana)
  # a) dla sz. szczegółowego
  q_szcz1 = quantile(dane1,na.rm=TRUE)
  q_szcz2 = quantile(dane2,na.rm=TRUE)

  # b) dla sz. rozdzielczego
  hist1$czest = cumsum(hist1$counts / sum(hist1$counts))
  q_rozdz1 = list(
    c("0%","25%","50%","75%","100%"),
    c(hist1$mids[hist1$czest >= 0][1],
      hist1$mids[hist1$czest >= 0.25][1],
      hist1$mids[hist1$czest >= 0.50][1],
      hist1$mids[hist1$czest >= 0.75][1],
      hist1$mids[hist1$czest >= 1][1]
    )
  )
  
  hist2$czest = cumsum(hist2$counts / sum(hist2$counts))
  q_rozdz2 = list(
    c("0%","25%","50%","75%","100%"),
    c(hist2$mids[hist2$czest >= 0][1],
      hist2$mids[hist2$czest >= 0.25][1],
      hist2$mids[hist2$czest >= 0.50][1],
      hist2$mids[hist2$czest >= 0.75][1],
      hist2$mids[hist2$czest >= 1][1]
    )
  )
  
  
  
  # Miary zróżnicowania - rozproszenia:
  # - wariancja(obc, nieobc)
  # szeregi szczegółowe
  war_nieobc_szcz1 = var(dane1)
  war_obc_szcz1 = war_nieobc_szcz1*dlugosc1/(dlugosc1-1)
  war_nieobc_szcz2 = var(dane2)
  war_obc_szcz2 = war_nieobc_szcz2*dlugosc2/(dlugosc2-1)
  
  # szeregi rozdzielcze
  war_nieobc_rozdz1 = sum(hist1$counts*(hist1$mids-sr_roz1)^2)/(dlugosc1-1)
  war_obc_rozdz1 = war_nieobc_rozdz1*dlugosc1/(dlugosc1-1)
  war_nieobc_rozdz2 = sum(hist2$counts*(hist2$mids-sr_roz2)^2)/(dlugosc2-1)
  war_obc_rozdz2 = war_nieobc_rozdz2*dlugosc2/(dlugosc2-1)
  
  # - odchylenie standardowe (obc, nieobc)
  # szeregi szczegółowe
  odch_nieobc_szcz1 = sqrt(war_nieobc_szcz1)
  odch_obc_szcz1 = sqrt(war_obc_szcz1)
  odch_nieobc_szcz2 = sqrt(war_nieobc_szcz2)
  odch_obc_szcz2 = sqrt(war_obc_szcz2)
  
  # szeregi rozdzielcze
  odch_nieobc_rozdz1 = sqrt(war_nieobc_rozdz1)
  odch_obc_rozdz1 = sqrt(war_obc_rozdz1)
  odch_nieobc_rozdz2 = sqrt(war_nieobc_rozdz2)
  odch_obc_rozdz2 = sqrt(war_obc_rozdz2)
  
  
  # - odchylenie przeciętne
  # szeregi szczegółowe
  odch_przec_szcz1 = sum(abs(dane1-sr_szcz1))/dlugosc1
  odch_przec_szcz2 = sum(abs(dane2-sr_szcz2))/dlugosc2
  
  # szeregi rozdzielcze
  odch_przec_rozdz1 = sum(hist1$counts*(abs(hist1$mids-sr_roz1)))/dlugosc1
  odch_przec_rozdz2 = sum(hist2$counts*(abs(hist2$mids-sr_roz2)))/dlugosc2
  
  # - odchylenie przeciętne od mediany
  # szeregi szczegółowe
  odch_przec_med_szcz1 = sum(abs(dane1-med_szcz1))/dlugosc1
  odch_przec_med_szcz2 = sum(abs(dane2-med_szcz2))/dlugosc2
  
  # szeregi rozdzielcze
  odch_przec_med_rozdz1 = sum(hist1$counts*(abs(hist1$mids-med_rozdz1)))/dlugosc1
  odch_przec_med_rozdz2 = sum(hist2$counts*(abs(hist2$mids-med_rozdz2)))/dlugosc2
  
  # - odchylenie ćwiartkowe Q
  # szeregi szczegółowe
  odch_q_szcz1 = (q_szcz1[4] - q_szcz1[2])/2
  odch_q_szcz2 = (q_szcz2[4] - q_szcz2[2])/2
  
  # szeregi rozdzielcze
  odch_q_rozdz1 = (q_rozdz1[[2]][[4]] - q_rozdz1[[2]][[2]])/2
  odch_q_rozdz2 = (q_rozdz2[[2]][[4]] - q_rozdz2[[2]][[2]])/2
  
  # - współczynnik zmienności w%v
  
  
  # - pozycyjny współczynnik zmienności w%v_q
  
  
  # Miary asymetrii i koncentracji:
  # - skośność a_s
  # - kurtoza krt
  # - eksces g_2
  
  #cumsum() - suma skumulowana
  #findInterval()
  
  #wyczysczenie pliku
  close( file( "polecenie2.txt", open="w" ) )
  #otwarcie pliku
  #wypisanie wyników
  plik2 = file("polecenie2.txt","a")
  cat("Wyniki dla polecenia 2.\n",file=plik2)
  
  #szeregi szczegółowe
  #walcownia 1
  cat("\nSzeregi szczegółowe:\n",file=plik2,append=TRUE)
  cat("Walcownia 1:\n",file=plik2,append=TRUE)
  #miary przeciętne
  cat(" Miary przeciętne:",file=plik2,"\n",append=TRUE)
  cat(" - średnia:",sr_szcz1,"\n",file=plik2,append=TRUE)
  cat(" - mediana:",med_szcz1,"\n",file=plik2,append=TRUE)
  cat(" - moda: -brak- (tylko dla sz. rozdzielczego)\n",file=plik2,append=TRUE)
  cat(" - kwartyle:\n",file=plik2,append=TRUE)
  write.table(q_szcz1,file=plik2,append=TRUE,quote=FALSE,sep="\t",col.names = FALSE)
  #miary zróżnicowania
  cat(" Miary zróżnicowania:\n",file=plik2,append=TRUE)
  cat(" - wariancja obciążona:",war_obc_szcz1,"\n",file=plik2,append=TRUE)
  cat(" - wariancja nieobciążona:",war_nieobc_szcz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie standardowe obciążone:",odch_obc_szcz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie standardowe nieobciążone:",odch_nieobc_szcz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie przeciętne:",odch_przec_szcz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie przeciętne od mediany:",odch_przec_med_szcz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie ćwiartkowe Q:",odch_q_szcz1,"\n",file=plik2,append=TRUE)
  
  #walcownia 2
  cat("Walcownia 2:\n",file=plik2,append=TRUE)
  #miary przeciętne
  cat(" Miary przeciętne:\n",file=plik2,append=TRUE)
  cat(" - średnia:",sr_szcz2,"\n",file=plik2,append=TRUE)
  cat(" - mediana:",med_szcz2,"\n",file=plik2,append=TRUE)
  cat(" - moda: -brak- (tylko dla sz. rozdzielczego)\n",file=plik2,append=TRUE)
  cat(" - kwartyle:\n",file=plik2,append=TRUE)
  write.table(q_szcz2,file=plik2,append=TRUE,quote=FALSE,sep="\t",col.names = FALSE)
  #miary zróżnicowania
  cat(" Miary zróżnicowania:\n",file=plik2,append=TRUE)
  cat(" - wariancja obciążona:",war_obc_szcz2,"\n",file=plik2,append=TRUE)
  cat(" - wariancja nieobciążona:",war_nieobc_szcz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie standardowe obciążone:",odch_obc_szcz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie standardowe nieobciążone:",odch_nieobc_szcz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie przeciętne:",odch_przec_szcz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie przeciętne od mediany:",odch_przec_med_szcz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie ćwiartkowe Q:",odch_q_szcz2,"\n",file=plik2,append=TRUE)
  
  
  #szeregi rozdzielcze
  #walcownia 1
  cat("\nSzeregi rozdzielcze:\n",file=plik2,append=TRUE)
  cat("Walcownia 1:\n",file=plik2,append=TRUE)
  #miary przeciętne
  cat(" Miary przeciętne:",file=plik2,"\n",append=TRUE)
  cat(" - średnia:",sr_roz1,"\n",file=plik2,append=TRUE)
  cat(" - mediana:",med_rozdz1,"\n",file=plik2,append=TRUE)
  cat(" - moda:",med_rozdz1,"\n",file=plik2,append=TRUE)
  cat(" - kwartyle:\n",file=plik2,append=TRUE)
  write.table(q_rozdz1,file=plik2,append=TRUE,quote=FALSE,sep="\t",col.names = FALSE,row.names = FALSE)
  #miary zróżnicowania
  cat(" Miary zróżnicowania:\n",file=plik2,append=TRUE)
  cat(" - wariancja obciążona:",war_obc_rozdz1,"\n",file=plik2,append=TRUE)
  cat(" - wariancja nieobciążona:",war_nieobc_rozdz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie standardowe obciążone:",odch_obc_rozdz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie standardowe nieobciążone:",odch_nieobc_rozdz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie przeciętne:",odch_przec_rozdz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie przeciętne od mediany:",odch_przec_med_rozdz1,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie ćwiartkowe Q:",odch_q_rozdz1,"\n",file=plik2,append=TRUE)
  
  #walcownia 2
  cat("\nSzeregi rozdzielcze:\n",file=plik2,append=TRUE)
  cat("Walcownia 1:\n",file=plik2,append=TRUE)
  #miary przeciętne
  cat(" Miary przeciętne:",file=plik2,"\n",append=TRUE)
  cat(" - średnia:",sr_roz2,"\n",file=plik2,append=TRUE)
  cat(" - mediana:",med_rozdz2,"\n",file=plik2,append=TRUE)
  cat(" - moda:",med_rozdz2,"\n",file=plik2,append=TRUE)
  cat(" - kwartyle:\n",file=plik2,append=TRUE)
  write.table(q_rozdz2,file=plik2,append=TRUE,quote=FALSE,sep="\t",col.names = FALSE,row.names = FALSE)
  #miary zróżnicowania
  cat(" Miary zróżnicowania:\n",file=plik2,append=TRUE)
  cat(" - wariancja obciążona:",war_obc_rozdz2,"\n",file=plik2,append=TRUE)
  cat(" - wariancja nieobciążona:",war_nieobc_rozdz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie standardowe obciążone:",odch_obc_rozdz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie standardowe nieobciążone:",odch_nieobc_rozdz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie przeciętne:",odch_przec_rozdz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie przeciętne od mediany:",odch_przec_med_rozdz2,"\n",file=plik2,append=TRUE)
  cat(" - odchylenie ćwiartkowe Q:",odch_q_rozdz2,"\n",file=plik2,append=TRUE)
  
  
  
  
  #zamkniecie pliku
  close(plik2)
  
}

