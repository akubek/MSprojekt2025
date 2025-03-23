y1=read.csv2("walc1.txt",header=FALSE)
y1=t(y1)
y2=read.csv2("walc2.txt",header=FALSE)
y2=t(y2)

data_list=list(y1,y2)

boxplot(data_list,names = c("Plik 1", "Plik 2"),
        main = "Wykres pudelkowy - Mediana i Kwartale",
        ylab = "Wartosci", col = c("lightblue", "lightgreen"))



