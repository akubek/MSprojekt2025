#Czy na poziomie istotności 0,05 można twierdzić, że wytrzymałości na rozciąganie w drugiej 
#walcowni są mniejsze (sformułować i zweryfikować odpowiednią hipotezę)?

polec6 <- function(dane1,dane2) {
  #Test t Welcha
  alpha = 0.05
  n1 = length(dane1)
  n2 = length(dane2)
  
  #srednia
  sr1 = mean(dane1)
  sr2 = mean(dane2)
  
  #war
  war1 = var(dane1)
  war2 = var(dane2)
  
  #hipoteza 0: walcownia 1 > walcownia 2
  #statystyka t 
  t = (sr1-sr2)/sqrt((war1/n1) + (war2/n2))
  
  #liczba stopni swobody przybliżona za pomocą równania Welcha-Satterthwaite'a
  v = round((((war1/n1) + (war2/n2))^2)/((war1^2)/((n1^2)*(n1-1)) + (war2^2)/((n2^2)*(n2-1))))
  
  #przedzial
  t_stud = qt(1-alpha,v)
  
  
  #sprawdzenie hipotezy
  cat("Przy poziomie istotności 0.05, przeciał krytyczny to <",t_stud,"; +inf)\n")
  cat("t = ",t,"\n")
  if(t < t_stud) {
    cat("Wartość nie należy do przedziału krytycznego - brak podstaw do odrzucenia hipotezy zerowej.\n")
  } else {
    cat("Wartość należy do przedziału krytycznego - odrzucamy hipotezę zerową na korzyść hipotezy alternatywnej.\n")
  }
}