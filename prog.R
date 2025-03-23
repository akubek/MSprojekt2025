y1=read.csv2("walc1.txt",header=FALSE)
y1=t(y1)
y2=read.csv2("walc2.txt",header=FALSE)
y2=t(y2)

data_list=list(y1,y2)

#wykres pudełkowy 2 zerstawów danych
#wartość średnia, kwartale
boxplot(data_list,names = c("Plik 1", "Plik 2"),
        main = "Wykres pudelkowy - Mediana i Kwartale",
        ylab = "Wartosci", col = c("lightblue", "lightgreen"))


#średnia i odchylenie standardowe
sr1=mean(y1)
sr2=mean(y2)
odch1=sd(y1)
odch2=sd(y2)

srednie=c(sr1,sr2)
odchylenia=c(odch1,odch2)

plot(1:2,srednie,
     main="Srednia i Odchylenie Standardowe",
     xlab="Pliki",
     ylab="Wartosci",
     xaxt="n",
     ylim=c(min(srednie-odchylenia),max(srednie+odchylenia)))
arrows(x0 = 1:2,
       y0 = srednie-odchylenia,
       x1 = 1:2,
       y1 = srednie+odchylenia,
       angle = 90, code = 3, length = 0.1, col = "red")
axis(1,at=1:2,labels=c("Plik 1", "Plik 2"))


