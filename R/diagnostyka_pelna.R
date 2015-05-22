#' @title Pełna diagnostyka wielomianow
#' @description Funkcja wykonuje pełną diagnostykę modeli EWD.
#' @param dane ramka danych z wynikami egzaminu
#' @param stopnie wektor liczb całkowitych określających stopnie wielomianów, dla których należy zbudować modele.
#' @param typModelu zmienna określająca, jaki model ma być użyty. Dopuszczalne wartości to "lm" oraz "lmer".
#' @param efektLosowy nazwa kolumny ramki 'dane', która określa efekt losowy. Parametr ignorowany dla modelu 'lm'.
#' @param egzWej nazwa kolumny ramki 'dane', która zawiera wyniki(umiejętności) egzaminu wejściowego.
#' @param egzWyj nazwa kolumny ramki 'dane', która zawiera wyniki(umiejętności) egzaminu wyjściowego.
#' @param zmGrupujace wektor ciągów znakowych określających zmienne modelu, na podstawie których określone są grupy, w których wykonywane są obliczenia oraz dla których są sporządzane wykresy
#' @param dysl_wej nazwa kolumny ramki 'dane', która zawiera informacje o dysleksji zgłoszonej dla egzaminu wejściowego.
#' @param dysl_wyj nazwa kolumny ramki 'dane', która zawiera informacje o dysleksji zgłoszonej dla egzaminu wyjściowego.
#' @param katalogRysunki ścieżka do katalogu do zapisu wykresów diagnostycznych.
#' @param porownanieModeliPlik ścieżka do pliku do zapisu porównania modeli. Parametr użyty tylko, gdy rozważamy wiele modeli.  
#' @param tytulDiagnostyka tytuł wykresu diagnostycznego rysowanego, gdy rozważany jest tylko jeden model.
#' @param maska wektor boolowski określający, które obserwacje z ramki dane powinny zostać użyte do wyestymowania modeli.
#' @return funkcja zwraca listę modeli lub model (w przypadku podania parametru 'stopnie' o długości 1).
#' @import lme4
#' @export
diagnostyka_pelna <- function(dane, stopnie, typModelu, efektLosowy, egzWej, egzWyj, zmGrupujace, dysl_wej, dysl_wyj, 
                              katalogRysunki, porownanieModeliPlik, tytulDiagnostyka, maska = NULL){
  
  if(any( ni <- stopnie%%1 != 0)){
    stop("Stopnie nie są liczbami całkowitymi:", stopnie[ni])
  }
  
  if(typModelu == "lmer" & is.null(efektLosowy)){
    stop("Nieokreślony efekt losowy dla modelu lmer.")
  }
  
  if(is.null(maska)){
    daneModel = dane
  } else {
    daneModel = subset(dane, maska)
  }
  
  modele = list()
  for(ind in seq_along(stopnie)){
    
    if(typModelu == "lm"){
      formulaTekst = paste0(egzWyj,"~","poly(", egzWej, ", ", stopnie[ind], ", raw=TRUE)*", 
                            paste(zmGrupujace, collapse = "*"),"+plec+", dysl_wyj, "*",dysl_wej )
      modele[[ind]] = lm(as.formula(formulaTekst), daneModel)
    } else if(typModelu == "lmer"){
      
      formulaTekst = paste0(egzWyj,"~","poly(", egzWej, ", ", stopnie[ind], ", raw=TRUE)*", 
                            paste(zmGrupujace, collapse = "*"),"+plec+", dysl_wyj, "*",dysl_wej ,
                            "+ (1 | ", efektLosowy, ")")
      
      modele[[ind]] = lmer(as.formula(formulaTekst), data=daneModel, REML=TRUE)
    }
    
    names(modele)[ind] = paste0("stopień ", stopnie[ind])
  }
  
  if(length(stopnie)>1){
    wlasnosciModeli = lapply(modele, diagnostyka_wielomianow, zmWielomian=egzWej, zmGrupujace=zmGrupujace, folderWykresy = katalogRysunki)
    porownaj_wielomiany(wlasnosciModeli, porownanieModeliPlik)
  } else if (length(stopnie) == 1) {
    diagnostyka(modele[[1]], katalogRysunki, tytulDiagnostyka)
    wlasnosciModeli = NULL
    modele = modele[[1]]
  }
  
  return(modele)
}