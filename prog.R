y1=read.csv2("walc1.txt",header=FALSE)
y1=t(y1)
y2=read.csv2("walc2.txt",header=FALSE)
y2=t(y2)

#Polecenie 1
source("polecenie_1.R")
polec1(y1,y2)

#Polecenie 2
source("polecenie_2.R")
polec2(y1,y2)


#Polecenie 3
source("polecenie_3.R")
polec3(y1,y2)

#Polecenie 4
source("polecenie_4.R")
polec4(y1,y2)

#Polecenie 5
source("polecenie_5.R")
polec5(y1,y2)

#Polecenie 6
source("polecenie_6.R")
polec6(y1,y2)

#Polecenie 7
source("polecenie_7.R")
polec7(y1,y2)
