#Czy na poziomie istotności 0,05 można twierdzić, że wartość przeciętna wytrzymałości na
#rozciąganie w pierwszej walcowni jest równa 650?

polec4 <- function(dane1,dane2) {
  #poziom istotności alph = 0.05
  #hipoteza zerowa - m0 = 650
  m0 = 650
  #t_stud1 = t(0.975,50-1)
  t_stud1 = 2.01
  #t_stud2 = t(0.975,48-1)
  t_stud2 = 2.012
  
  #srednia
  sr1 = mean(dane1)
  sr2 = mean(dane2)
  
  dl1 = length(dane1)
  dl2 = length(dane2)
  #odchylenie standardowe - obciazone
  sd1 = sd(dane1)
  sd2 = sd(dane2)
  
  #statystyka testowa t-studenta
  t1 = (sr1 - m0)*(sqrt(dl1))/sd1
  t2 = (sr2 - m0)*(sqrt(dl2))/sd2
  
  
  close( file( "polecenie4.txt", open="w" ) )
  #otwarcie pliku
  #wypisanie wyników
  plik4 = file("polecenie4.txt","a")
  cat("Wyniki dla polecenia 4.\n",file=plik4)
  
  cat("Porównanie wartości krytycznych:\n",file=plik4,append=TRUE)
  cat("dla walcowni 1:\n",file=plik4)
  cat("wartość krytyczna t_alpha/2:",t_stud1," \n",file=plik4)
  cat("srednia:",sr1," \n",file=plik4)
  cat("odchylenie standardowe - nieobciazone:",sd1," \n",file=plik4)
  cat("statystyka testowa t:",t1," \n",file=plik4)
  if(t1 < -t_stud1 || t1 > t_stud1 ) {
    #odrzucamy hipotezę zerową
    cat("Statystyka testowa należy do przedziału krytycznego więc odrzucamy hipotezę zerową na korzyść hipotezy alternatywnej.\n",file=plik4)
  } else {
    cat("Statystyka testowa nie należy do przedziału krytycznego - brak podstaw do odrzucenia hipotezy zerowej.\n",file=plik4)
  }
  cat("\ndla walcowni 2:\n",file=plik4)
  cat("wartość krytyczna t_alpha/2:",t_stud2," \n",file=plik4)
  cat("srednia:",sr2," \n",file=plik4)
  cat("odchylenie standardowe - nieobciazone:",sd2," \n",file=plik4)
  cat("statystyka testowa t:",t2," \n",file=plik4)
  if(t2 < -t_stud2 || t2 > t_stud2 ) {
    #odrzucamy hipotezę zerową
    cat("Statystyka testowa należy do przedziału krytycznego więc odrzucamy hipotezę zerową na korzyść hipotezy alternatywnej.\n",file=plik4)
  } else {
    cat("Statystyka testowa nie należy do przedziału krytycznego - brak podstaw do odrzucenia hipotezy zerowej.\n",file=plik4)
  }
  close(plik4)
}