y1=read.csv2("walc1.txt",header=FALSE)
y1=t(y1)
y2=read.csv2("walc2.txt",header=FALSE)
y2=t(y2)

data_list=list(y1,y2)

#POLECENIE 1

#wykres pudełkowy 2 zestawów danych
#mediana, kwartale
png("p1-mediana_i_kwartyle.png",width=800,height=800)
boxplot(data_list,names = c("Plik 1", "Plik 2"),
        main = "Wykres pudelkowy - Mediana i Kwartale",
        ylab = "Wartosci", col = c("lightblue", "lightgreen"))
dev.off()


#średnia i odchylenie standardowe
#wartość oczekiwana rozkładu normalnego - średnia
sr1=mean(y1)
sr2=mean(y2)
odch1=sd(y1)
odch2=sd(y2)

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
png("p1-wartosc_oczekiwana_i_odchylenie_standardowe.png",width=800,height=800)
boxplot(wartosci,names = c("Plik 1", "Plik 2"),
        main = "Wykres pudelkowy - Średnia i odchylenie standardowe",
        ylab = "Wartosci", col = c("lightblue", "lightgreen"))
dev.off()


#wykres zwykły
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

#POLECENIE 2

#szreg rozdzielczy
#il. grup - pierw z l. obserwacji
b1 = as.integer(sqrt(length(y1)))
b2 = as.integer(sqrt(length(y2)))
#histogram - hist()
hist1 = hist(y1,breaks=b1)
hist2 = hist(y2,breaks=b2)
#which.max() - moda
moda1 = hist1$mids[which.max(hist1$counts)]
moda2 = hist2$mids[which.max(hist2$counts)]

# Miary przecietne - położenia:
# - srednia
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
# - mediana
mediana1 = median(y1)
mediana2 = median(y2)
# - moda
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

sprintf("Srednia dla szeregu szczegolowego, sr1 = %f, sr2 = %f",sr1, sr2)
sprintf("Srednia dla szeregu rozdzielczego, sr1 = %f, sr2 = %f",sr_roz1, sr_roz2)
sprintf("Mediana dla szeregu szczegolowego, mediana1 = %f, mediana2 = %f",mediana1, mediana2)
sprintf("Mediana dla szeregu rozdzielczego, sr1 = %f, sr2 = %f",sr_roz1, sr_roz2)




