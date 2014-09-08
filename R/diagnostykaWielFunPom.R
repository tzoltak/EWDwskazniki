#' @title Pobieranie nazw zmiennych z formuł
#' @description
#' Funkcja pomocnicza, która zwraca nazwy prawostronnych zmiennych formuły. Jeżeli formuła jest obustronna to funkcja zwraca błąd.
#' @param formula parametr klasy 'formula'.
#' @param nazwa ciąg znaków wykorzystywany w komunikcie błedu.
#' @param czyJednaZmienna zmienna logiczna. Jeżeli wartość wynosi TRUE to funkcja zwraca błąd dla formuł prawostronnych, które 
#' zawierają więcej niż jedną zmienną.
#' @return wektor ciągów znakowych, które są nazwami prawostronnych zmiennych formuły.
# #' @import formula.tools
wyciagnij_nazwe_zmiennej <- function(formula, 
                                     nazwa = as.character(substitute(formula)),
                                     czyJednaZmienna = TRUE
){
  if(is.null(formula)){
    return(formula)
  }
  
  if( class(formula) == "formula" ){
    if(!is.one.sided(formula)  ) {
      stop(paste0("Formuła ", nazwa, " nie jest prawostronna."))
    }
    zmienne = all.vars(formula)
    if(czyJednaZmienna & length(zmienne) != 1){
      stop(paste0("Niepoprawna ilość zmiennych w formule ", nazwa, "."))
    }
    return(zmienne)
  }
  
  if(class(formula)=="character"){
    return(formula)
  }
  
  stop("Zmienna '", nazwa, "' nie jest ani obiektem klasy 'character', ani 'formula'." )
}
#' @title Pobieranie nazw zmiennych z formuł
#' @description
#' Funkcja pomocnicza, która zwraca nazwy prawostronnych zmiennych formuły. Jeżeli formuła jest obustronna to funkcja zwraca błąd.
#' @param dane tablica z danymi.
#' @param zmiennaGr wektor nazw kolumn tablicy dane, dla których mam
#' @return 
#' Funkcja zwraca tablicę, której wiersze zawierają wszystkie kombinacje poziomów zmiennych grupujacych.
daj_grupowanie <- function(dane, zmiennaGr){
  grupowanie = data.frame(levels(dane[, zmiennaGr[1]]))
  names(grupowanie) = zmiennaGr[1]
  
  for(i in seq_along(zmiennaGr)[-1]){
    tmpLevels = data.frame(levels(dane[, zmiennaGr[i]]))
    names(tmpLevels) = zmiennaGr[i]
    grupowanie =  merge(grupowanie, tmpLevels) 
  }
  return(grupowanie)
}
#' @title Wyliczenia dla wielomianu.
#' @description
#' Funkcja wykonująca wyliczenia dla wielomianowej części modelu w zadanej grupie obserwacji.
#' @param model obiekt klasy 'lm' lub 'lmerMod'.
#' @param zmienna ciąg znaków określający zmienną wielomianu.
#' @param zmiennaGr wektor ciągów znakowych okreslających zmienne modelu, na podstawie których określone są grupy, w których wykonywane są obliczenia oraz dla których są sporządzane wykresy.
#' @param grupa ciąg zmiennych logicznych określający, która obserwacja znajduje się w rozważanej grupie.
#' @param grupowanie tablica, która jest efektem działania funkcji \code{\link{daj_grupowanie}}.
#' @param indeksGrupowania indeks tablicy podawanej w parametrze 'grupowanie'. Indeks wskazuje bierzącą grupę.
#' @param nsiatka liczba punktów, w której ma zostać obliczony wielomian. Domyślna wartość 0 oznacza, że wielomian będzie obliczony dla każdej występującej wartości.
#' @param czyTylkoGrupa wartośc logiczna. Jeżeli wynosi TRUE to wartości oblicznanego wielomianu są między minimum i maksimum dla grupy.
#' Jeżeli wartość wynosi FALSE to minimum i maksimum jest liczone dla całej populacji.
#' @return 
#' Funkcja zwraca ramkę danych z wartościami zmiennej, obliczeniami reszt oraz wartościami przewidywań na podstawie wielomianu.
#' Atrybutami ramki danych zawierają informacje o wartościach współczynników przy kolejnych potęgach, informację czy funkcja jest roasnąca oraz 
#' nazwy zmiennych z ramki danych, które brały udział w obliczeniach współczynnika wielomianu.
wielomian_lmer <- function(model, zmienna, zmiennaGr, grupa, grupowanie, indeksGrupowania, nsiatka=0, czyTylkoGrupa = FALSE){
  
  isPoly = any(indPoly <- grepl("^poly[(]", colnames(modelFrame(model))))
  
  wartosciZmiennej = if(isPoly){
    modelFrame(model)[, indPoly][, 1]
  } else {
    modelFrame(model)[, zmienna]
  } 
  
  # efLos = ranef(model)
  # zmienne, które zawierają zmienną z wielomianu
  maska = paste0(
    "^(|.+:)(|I[(]|poly[(])",
    zmienna,
    "(|[ ^][[:digit:]]+[)]|,.*[)][[:digit:]]+)(|:.+)$"
  ) 
  zmiennaVal = grepl(maska,  colnames(model.matrix(model)))
  # zmiennaVal = grepl(zmienna, colnames(model.matrix(model)))
  
  # wyrażenia związane z grupowaniem 
  expr = if( ncol(grupowanie)==1 ){
    paste0(names(grupowanie), as.character(grupowanie[indeksGrupowania, ]))
  } else{
    paste0(names(grupowanie), apply(grupowanie[indeksGrupowania, ], 2, as.character))
  }
  # liczba dwukropków
  colons = sapply(strsplit(colnames(model.matrix(model)), ":"), length) - 1
  
  # wystapieniaExpr - zawierają liczbę wystąpień w nazwach zmiennych modelu parametrow expr
  wystapieniaExpr = numeric(length(colons))
  for(ii in seq_along(expr)){
    wystapieniaExpr = wystapieniaExpr + grepl(expr[ii], colnames(model.matrix(model)))
  }
  
  # zmienneLegalne - to zmienne, które należy uwzględnić przy wyliczaniu wielomianu (ale bez interceptów)
  zmienneLegalne = colnames(model.matrix(model))[zmiennaVal & wystapieniaExpr==colons]
  
  # wektor, dla którego chcemy obliczyć wielomian
  siatka  = if(nsiatka <= 0){
    if(!czyTylkoGrupa){
      sort(unique(wartosciZmiennej))
    } else{
      sort(unique(wartosciZmiennej[grupa]))
    }
  } else{
    if(!czyTylkoGrupa){
      seq(min(wartosciZmiennej),max(wartosciZmiennej), length.out= nsiatka)
    } else{
      seq(min(wartosciZmiennej[grupa]),max(wartosciZmiennej[grupa]), length.out= nsiatka)
    }
  } 
  
  coefs = fixef(model)
  # intercept uwzględnia również parametry związane z grupowaniem.
  colCoefs = colnames(coefs) %in% expr
  intercept = fixef(model)["(Intercept)"] + ifelse(any(colCoefs), sum(coefs[colCoefs]), 0)
  
  maska1st = paste0("^", zmienna, "|^poly[(]",zmienna,",.*[)]1")
  # zmienneLegalne[grepl(maska1st, zmienneLegalne)]
  
  zmienne1st = zmienneLegalne[grepl(maska1st, zmienneLegalne)]
  cat("\n", zmienne1st)
  wspolczynniki = c(intercept, sum(coefs[zmienne1st]))
  YY = sum(coefs[zmienne1st]) * siatka + intercept
  
  N=2
  while(TRUE){
    maskaNst = paste0(zmienna, "\\^", N,"|^poly[(]",zmienna,",.*[)]",N)
    zmienneNst = zmienneLegalne[grepl(maskaNst, zmienneLegalne)]
    cat("\n", zmienneNst)
    if(length(zmienneNst)==0){
      break
    }
    wspolczynniki = c(wspolczynniki, sum(coefs[zmienneNst]))
    YY = YY + sum(coefs[zmienneNst])*(siatka^N)
    N = N + 1
  }
  # stopień wielomianu
  # degree = N - 1
  
  #   monot=c(ret[2]>ret[1], ret[-1]>ret[-length(ret)])
  #   all(monot)
  ret = data.frame(x=siatka, y=YY)
  
  monot = c(YY[2]>YY[1], YY[-1]>YY[ -length(YY)])
  attributes(ret)$czyRosnaca=all(monot)
  attributes(ret)$wspolczynniki = wspolczynniki 
  
  interceptNames = if(any(colCoefs)){
    names(coefs)[colCoefs]
  } else{
    NULL
  }
  attributes(ret)$zmienne = c("(Intercept)", interceptNames, zmienneLegalne)
  class(ret) = "wielomian_lmer"
  return(ret)
}

oblicz_wartosci_wielomianu <- function(wektor, wielomian){
  wspolczynniki = attributes(wielomian)$wspolczynniki 
  ret = wektor*wspolczynniki[2] + wspolczynniki[1]
  
  for( i in seq_along(wspolczynniki[-1:-2])){
    ret = ret + wspolczynniki[2+i]*(wektor^(1+i))
  }
  return(ret)
}

#  ktore_naleza_do_grupy(daneGrupujace = modelFrame(model)[, zmiennaGr], grupowanie, indeksGrupowania)
ktore_naleza_do_grupy <- function(daneGrupujace, grupowanie, indeksGrupowania){
  if(is.null(grupowanie) | nrow(grupowanie) ==0){
    return(rep(TRUE, nrow(daneGrupujace)))
  }
  
  if(ncol(grupowanie)==1){
    return( daneGrupujace==grupowanie[indeksGrupowania, ])
  }
  
  return( apply(daneGrupujace, 1, function(x){all(x==grupowanie[indeksGrupowania, ])}))
}


lines.wielomian_lmer <- function(wielomian, colRos=3, colMal =4, lwd=2, lty=1){
  monot = c(wielomian$y[2]>wielomian$y[1], wielomian$y[-1]>wielomian$y[ -length(wielomian$y)])
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

przewidywanie_lmer <- function(model, wielomian, grupa, zmienna){
  isPoly = any(indPoly <- grepl("^poly[(]", colnames(modelFrame(model))))
  
  zmiennaZalezna = all.vars(attributes(modelFrame(model))$terms)[1]
  zm = attributes(wielomian)$zmienne
  przewidywaniaWielomianem = (model.matrix(model)[grupa, zm] %*% (fixef(model)[zm]))
  
  zmNeg = colnames(model.matrix(model))[! colnames(model.matrix(model)) %in% zm]  
  efStale = (model.matrix(model)[grupa, zmNeg] %*% (fixef(model)[zmNeg]))[, 1]
  
  efLos = ranef(model)
  efLos = mapply(function(x, nazwa)return(setNames(data.frame(rownames(x), x$"(Intercept)"), c(nazwa, paste0("efLos_",nazwa)))),     
                 efLos, as.list(names(efLos)), SIMPLIFY=FALSE)
  
  if(isPoly){
    zmiennaX = colnames(model.matrix(model))[ grepl(paste0("^poly[(]", zmienna,", [[:digit:]]+, raw = TRUE[)]1$") , colnames(model.matrix(model)))]
    temp = cbind( modelFrame(model)[grupa, c(names(efLos), zmiennaZalezna) ], model.matrix(model)[grupa, zmiennaX])
    colnames(temp) = c(names(efLos), zmiennaZalezna,  zmienna)
  } else{
    temp = modelFrame(model)[grupa, c(names(efLos), zmiennaZalezna, zmienna) ]
  }
  colnames(temp)[seq_along(efLos)] = names(efLos)
  
  #temp = modelFrame(model)[grupa, c(names(efLos), zmiennaZalezna, zmiennaX) ] 
  
  for (i in seq_along(efLos)) temp=merge(temp, efLos[[i]])
  
  sumaEfLos = if( is.null(efLos) | length(efLos) ==0  ){
    rep(0, nrow(temp))
  } else{
    apply(as.data.frame(temp[, paste0("efLos", "_", names(efLos) )]),1,sum)
  }
  
  reszty = modelFrame(model)[grupa, zmiennaZalezna] - (efStale) - sumaEfLos
  temp = cbind(temp, reszty, wielomian = przewidywaniaWielomianem)
  colnames(temp)[ncol(temp)] = "wielomian"
  # temp$wielomian = przewidywaniaWielomianem
  return(data.frame(temp))
}

pseudoR2 <- function(model){
  UseMethod("pseudoR2")
}

pseudoR2.lm <- function(model){
  return(summary(model)$r.squared)
}

# wariancja największej wiarygodności
pseudoR2.lmerMod <- function(model){
  efLos = ranef(model)
  ret = c(sapply(VarCorr(model), function(x) x[[1]]), attributes(VarCorr(model))$sc^2)
  
  dt <- data.frame(pred = predict(model, re.form = ~0), efekt =modelFrame(model)[, names(efLos)])
  
  myVar <- function(x) {
    m = mean(x)
    ret = sum((x-m)^2)/length(x)
    return(ret)
  }
  
  tab = ddply(dt, ~efekt, summarise, mean=mean(pred), var=myVar(pred), n = length(pred))
  
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
  UseMethod("modelFrame")
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










