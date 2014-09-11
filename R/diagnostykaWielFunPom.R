#' @title Pobieranie nazw zmiennych z formul
#' @description
#' Funkcja pomocnicza, która zwraca nazwy prawostronnych zmiennych formuły. Jeżeli formuła jest obustronna to funkcja zwraca błąd.
#' @param formula parametr klasy 'formula'.
#' @param nazwa ciąg znaków wykorzystywany w komunikcie błedu.
#' @param czyJednaZmienna zmienna logiczna. Jeżeli wartość jest równa TRUE, to funkcja zwraca błąd dla formuł prawostronnych, które 
#' zawierają więcej niż jedną zmienną.
#' @import formula.tools
#' @return wektor ciągów znakowych, które są nazwami prawostronnych zmiennych formuły.
wyciagnij_nazwe_zmiennej <- function(formula, 
                                     nazwa = as.character(substitute(formula)),
                                     czyJednaZmienna = TRUE
){
  if(is.null(formula)){
    return(formula)
  } else if( class(formula) == "formula" ){
    if(!is.one.sided(formula)  ) {
      stop(paste0("Formuła ", nazwa, " nie jest prawostronna."))
    }
    zmienne = all.vars(formula)
    if(czyJednaZmienna & length(zmienne) != 1){
      stop(paste0("Niepoprawna liczba zmiennych w formule ", nazwa, "."))
    }
    return(zmienne)
  } else if (class(formula) == "character") {
    return(formula)
  } else {
    stop("Zmienna '", nazwa, "' nie jest ani obiektem klasy 'character', ani 'formula'." )
  }
}
#' @title Wyliczenia dla wielomianu.
#' @description
#' Funkcja wykonująca wyliczenia dla wielomianowej części modelu w zadanej grupie obserwacji.
#' @param matrix macierz danych badanego modelu.
#' @param fixefModel wektor współczynników efektów stałych modelu.
#' @param zmienna ciąg znaków określający zmienną wielomianu.
#' @param poziomyGrup wiersz ramki danych z poziomami, dla której wykonywane są obliczenia.
#' Nazwy kolumn opisują zmienne.
#' @return  
#' Funkcja zwraca ramkę danych z wartościami zmiennej, obliczeniami reszt oraz wartościami przewidywań na podstawie wielomianu.
#' Atrybuty ramki danych zawierają informacje o wartościach współczynników przy kolejnych potęgach, informację czy funkcja jest roasnąca oraz 
#' nazwy zmiennych z ramki danych, które brały udział w obliczeniach współczynnika wielomianu.
wielomian_grupa <- function(matrix, fixefModel, zmienna, poziomyGrup){
  namesMatrix = colnames(matrix)
  
  if(is.null(poziomyGrup)){
    zmiennaGr = NULL
  } else {
    zmiennaGr = colnames(poziomyGrup)
  }
  
  indZmienna <- grepl(paste0("^poly[(]", zmienna, ", [[:digit:]]+(|, raw = TRUE)[)]1$|^", zmienna, "$"), namesMatrix)
  wartosciZmiennej = matrix[, indZmienna]
  
  # zmienne, które zawierają zmienną z wielomianu
  maska = paste0(
    "^(|.+:)(|I[(]|poly[(])",
    zmienna,
    "(|[ ^][[:digit:]]+[)]|,.*[)][[:digit:]]+)(|:.+)$"
  ) 
  # zmiennaVal = grepl(maska,  namesMatrix)
  nazwyZeZmienna = grepl(maska,  namesMatrix) & !apply(matrix, 2, function(x){ all(x==0)} )
  
  # wyrażenia związane z grupowaniem. Zostawiłem je ponieważ wyłapywanie zmiennych przez grepl wiązałoby się z ryzykiem 
  # złapania zmiennych, których nazwy zaczynają się od nazw zmiennych grupujących.
  if(is.null(poziomyGrup)) {
    expr =  NULL
  } else if( length(poziomyGrup)==1 ){
    expr = paste0(names(poziomyGrup), as.character(poziomyGrup))
  } else{
    expr = paste0(names(poziomyGrup), apply(poziomyGrup, 2, as.character))
  }
  
  maskaIntercept = ifelse(is.null(expr), "^[(]Intercept[)]$", 
                          paste0("^[(]Intercept[)]$|^", paste0(expr, collapse= ".*$|^"), ".*$"))
  
  nazwyIntercept = grepl(maskaIntercept,  namesMatrix) & !grepl("[:]+", namesMatrix)
  
  nazwy = c(namesMatrix[nazwyIntercept], namesMatrix[nazwyZeZmienna])
  
  wartosciWielomianu = matrix[, nazwy]%*%fixefModel[nazwy]
  wielomianXY = unique(data.frame(x = wartosciZmiennej, y = wartosciWielomianu ))
  wielomianXY = wielomianXY[order(wielomianXY$x), ]
  
  intercept = sum(fixefModel[nazwyIntercept])
  
  maska1st = paste0("^", zmienna, "|^poly[(]", zmienna, ", [[:digit:]]+(|, raw = TRUE)[)]1$")
  zmienne1st = namesMatrix[nazwyZeZmienna][grepl(maska1st, namesMatrix[nazwyZeZmienna])]
  
  wspolczynniki = c(intercept, sum(fixefModel[zmienne1st]))
  
  N=2
  while(TRUE){
    maskaNst = paste0("I[(]", zmienna, "\\^", N,"[)]|^poly[(]", zmienna, ", [[:digit:]]+(|, raw = TRUE)[)]", N)
    zmienneNst = namesMatrix[nazwyZeZmienna][grepl(maskaNst, namesMatrix[nazwyZeZmienna])]
    if(length(zmienneNst)==0){
      break
    }
    wspolczynniki = c(wspolczynniki, sum(fixefModel[zmienneNst]))
    N = N + 1
  }
  
  monot = c(wielomianXY$y[2]>wielomianXY$y[1], wielomianXY$y[-1]>wielomianXY$y[ -length(wielomianXY$y)])
  attributes(wielomianXY)$wartosciWielomianu = wartosciWielomianu
  attributes(wielomianXY)$czyRosnaca = monot
  attributes(wielomianXY)$wspolczynniki = wspolczynniki 
  attributes(wielomianXY)$zmienne = nazwy
  attributes(wielomianXY)$zmiennaX = zmienna
  class(wielomianXY) = "wielomian"
  return(wielomianXY)
}
#' @title Przeciążenie funkcji lines dla obiektu wielomian.
#' @description
#' Funkcja dodaje od wykresy linię wielomianu. 
#' @param wielomian obielt klasy 'wielomian'.
#' @param colRos kolor rosnącej części wykresu wielomianu.
#' @param colMal kolor malejacej części wykresu wielomianu.
#' @param lwd szerokość linii. Patrz również \code{\link[graphics]{par}}.
#' @param lty typ linii. Patrz również \code{\link[graphics]{par}}.
#' @return Funkcja nic nie zwraca.
lines.wielomian <- function(wielomian, colRos=3, colMal =4, lwd=2, lty=1){
  monot = attributes(wielomian)$czyRosnaca
  lines(wielomian$x[monot], wielomian$y[monot], col=colRos, lwd=lwd, lty=lty)
  lines(wielomian$x[!monot], wielomian$y[!monot], col=colMal, lwd=lwd, lty=lty)
  
  inds = which(! monot[-1]==monot[-length(monot)])
  
  for(i in seq_along(inds)){
    if(wielomian$y[inds[i]] < wielomian$y[inds[i]+1]){
      lines(wielomian$x[inds[i]:(inds[i]+1)], wielomian$y[inds[i]:(inds[i]+1)], col=colRos, lwd=lwd, lty=lty)
    } else{
      lines(wielomian$x[inds[i]:(inds[i]+1)], wielomian$y[inds[i]:(inds[i]+1)], col=colMal, lwd=lwd, lty=lty)
    }
  }
}
#' @title Obliczanie reszt dla grupy
#' @description
#' Funkcja oblicza reszt związane z efektami stałymi.
#' @param wielomian obiekt klasy 'wielomian'.
#' @param matrix macierz modelu.
#' @param frame wejściowa ramka danych do modelu.
#' @param ranefModel lista efektóe losowych modelu. 
#' @param fixefModel lista efektów stałych
#' @return Funkcja zwraca ramkę danych, która zawiera informacje dla obserwacji o:
#' grupowaniu, zmiennej niezależnej, efektach losowych, resztach oraz efetach związanych z wielomianem.
reszta_grupa <- function(wielomian, matrix, frame, ranefModel, fixefModel){
  
  zmienna = attributes(wielomian)$zmiennaX
  namesMatrix = colnames(matrix)
  zmiennaZalezna = all.vars(attributes(frame)$terms)[1]
  zmiennaMatrix = namesMatrix[ grepl(paste0("^", zmienna, "$|^poly[(]", zmienna,", [[:digit:]]+(|, raw = TRUE)[)]1$") , namesMatrix)]
  
  zm = attributes(wielomian)$zmienne
  przewidywaniaWielomianem = attributes(wielomian)$wartosciWielomianu
  
  zmNeg = namesMatrix[!namesMatrix %in% zm]  
  efStale = as.vector(matrix[, zmNeg] %*% (fixefModel[zmNeg]))
  
  efLos = mapply(function(x, nazwa)return(setNames(data.frame(rownames(x), x$"(Intercept)"), c(nazwa, paste0("efLos_",nazwa)))),     
                 ranefModel, as.list(names(ranefModel)), SIMPLIFY=FALSE)
  
  temp = data.frame( frame[, c(names(efLos)) ], matrix[, zmiennaMatrix])
  colnames(temp) = c(names(efLos), zmienna)
  
  for (i in seq_along(efLos)) temp=merge(temp, efLos[[i]])
  
  if( is.null(efLos) | length(efLos) ==0  ){
    sumaEfLos = rep(0, nrow(temp))
  } else{
    sumaEfLos = apply(as.data.frame(temp[, paste0("efLos", "_", names(efLos) )]), 1, sum)
  }
  
  reszty = frame[, zmiennaZalezna] - efStale - sumaEfLos
  temp = cbind(temp, reszty, wielomian = przewidywaniaWielomianem)
  colnames(temp)[ncol(temp)] = "wielomian"
  return(data.frame(temp))
}

pseudoR2 <- function(model){
  return(UseMethod("pseudoR2"))
}

pseudoR2.lm <- function(model){
  return(summary(model)$r.squared)
}

biasedVar <- function(x) {
  m = mean(x)
  ret = sum((x-m)^2)/length(x)
  return(ret)
}

#' @import plyr
pseudoR2.lmerMod <- function(model){
  efLos = ranef(model)
  ret = c(sapply(VarCorr(model), function(x) x[[1]]), attributes(VarCorr(model))$sc^2)
  
  dt <- data.frame(pred = predict(model, re.form = ~0), efekt =modelFrame(model)[, names(efLos)])
  
  pred = NULL # aby usunąć komunikat 'note' z check.
  tab = ddply(dt, ~efekt, summarise, mean=mean(pred), var=EWDwskazniki:::biasedVar(pred), n=length(pred))
  
  EEf = sum(tab$mean*tab$n)/sum(tab$n)
  varE = sum((tab$mean - EEf)^2*tab$n)/sum(tab$n)
  
  Evar = sum(tab$var*tab$n)/sum(tab$n)
  ret = cbind(ret, c(varE, Evar ))
  ret = cbind(ret, apply(ret, 1, sum))
  ret = rbind(ret, apply(ret, 2, sum))
  rownames(ret) <- c(names(efLos), "Resid.", "Suma")
  colnames(ret)<- c("Ef. Los", "Ef. St.", "Suma")
  return(ret)
}

modelFrame <- function(model){
  return(UseMethod("modelFrame"))
}

modelFrame.lmerMod <- function(model){
  return(model@frame)
}

modelFrame.lm <- function(model){
  return(model$model)
}

fixef.lm<-function(model){
  return(coefficients(model))
}

ranef.lm <- function(model){
  return(list())
}










