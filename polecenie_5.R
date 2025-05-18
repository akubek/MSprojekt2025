#Czy na poziomie istotności 0,05 można twierdzić, że odchylenie standardowe 
#wytrzymałości na rozciąganie w drugiej walcowni jest równe 64?

polec5 <- function(dane1,dane2) {
  #test chi^2
  alpha = 0.05
  n = length(dane2)
  chi1 = qchisq(1-alpha/2,n-1)
  chi2 = qchisq(alpha/2,n-1)
  
  #hipoteza zerowa
  odch0 = 64
  
  #hipoteza alternatywna
  #odch1 =/= 64
  
  #war nieobc
  war = var(dane1)
  
  #obliczenie przedziału
  min = sqrt((n-1)*war/chi1)
  max = sqrt((n-1)*war/chi2)
  
  #sprawdzenie hipotezy
  cat("Przy poziomie istotności 0.05, przeciał ufności to ",min," < odch0 < ",max,"\n")
  cat("odch0 = ",odch0,"\n")
  if(odch0 > min && odch0 < max) {
    cat("Wartość nie należy do przedziału krytycznego - brak podstaw do odrzucenia hipotezy zerowej.\n")
  } else {
    cat("Wartość należy do przedziału krytycznego - odrzucamy hipotezę zerową na korzyść hipotezy alternatywnej.\n")
  }
}