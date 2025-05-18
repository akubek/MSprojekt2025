#Czy na poziomie istotności 0,05 można twierdzić, że wartość przeciętna wytrzymałości na
#rozciąganie w pierwszej walcowni jest równa 650?

polec4 <- function(dane1,dane2) {
  #poziom istotności alph = 0.05
  alpha = 0.05
  #hipoteza zerowa - m0 = 650
  m0 = 650
  
  n = length(dane1)
  #t_stud1 = t(0.975,50-1)
  t_stud = abs(qt(1-alpha/2,n-1))
  #t_stud2 = t(0.975,48-1)
  #t_stud2 = 2.012
  
  #srednia
  sred = mean(dane1)
  
  #odchylenie standardowe - obciazone
  s = sd(dane1)
  
  #statystyka testowa t-studenta
  t = (sred - m0)*(sqrt(n))/s
  
  
  close( file( "polecenie4.txt", open="w" ) )
  #otwarcie pliku
  #wypisanie wyników
  plik4 = file("polecenie4.txt","a")
  cat("Wyniki dla polecenia 4.\n",file=plik4)
  
  cat("Porównanie wartości krytycznych:\n",file=plik4,append=TRUE)
  cat("dla walcowni 1:\n",file=plik4)
  cat("wartość krytyczna t_alpha/2:",t_stud," \n",file=plik4)
  cat("srednia:",sred," \n",file=plik4)
  cat("odchylenie standardowe - nieobciazone:",s," \n",file=plik4)
  cat("statystyka testowa t:",t," \n",file=plik4)
  if(t < -t_stud || t > t_stud ) {
    #odrzucamy hipotezę zerową
    cat("Statystyka testowa należy do przedziału krytycznego więc odrzucamy hipotezę zerową na korzyść hipotezy alternatywnej.\n",file=plik4)
  } else {
    cat("Statystyka testowa nie należy do przedziału krytycznego - brak podstaw do odrzucenia hipotezy zerowej.\n",file=plik4)
  }
  
  close(plik4)
}