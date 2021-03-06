#' @title Pobieranie nazw zmiennych z formul
#' @description
#' Funkcja pomocnicza, która zwraca nazwy prawostronnych zmiennych formuły.
#' Jeżeli formuła jest obustronna to funkcja zwraca błąd.
#' @param formula parametr klasy 'formula'.
#' @param nazwa ciąg znaków wykorzystywany w komunikcie błedu.
#' @param czyJednaZmienna zmienna logiczna. Jeżeli wartość jest równa TRUE, to
#' funkcja zwraca błąd dla formuł prawostronnych, które
#' zawierają więcej niż jedną zmienną.
#' @return wektor ciągów znakowych, które są nazwami prawostronnych zmiennych
#' formuły
#' @importFrom stats terms
wyciagnij_nazwe_zmiennej <- function(formula,
                                     nazwa = as.character(substitute(formula)),
                                     czyJednaZmienna = TRUE){
  if (is.null(formula)) {
    return(formula)
  } else if (class(formula) == "formula") {
    if (attributes(terms(formula) )$response != 0) {
      stop(paste0("Formuła ", nazwa, " nie jest prawostronna."))
    }
    zmienne = all.vars(formula)
    if (czyJednaZmienna & length(zmienne) != 1) {
      stop(paste0("Niepoprawna liczba zmiennych w formule ", nazwa, "."))
    }
    return(zmienne)
  } else if (class(formula) == "character") {
    return(formula)
  } else {
    stop("Zmienna '", nazwa, "' nie jest ani obiektem klasy 'character', ani 'formula'." )
  }
}

#' @title Wyliczenia do wspołczynnika R-kwadrat.
#' @description
#' Funkcja dla modeli 'lm' zwraca współczynnik R-kwadrat, dla modeli 'lmerMod'
#' zwraca tabicę dekompozycji wariancji na efekty stałe i losowe.
#' @param model parametr klasy 'lmerMod' lub 'lm'.
#' @return Funkcja zwraca liczbę lub macierz w zależności od klasy obiektu model
pseudoR2 <- function(model){
  return(UseMethod("pseudoR2"))
}

pseudoR2.lm <- function(model){
  return(summary(model)$r.squared)
}

#' @importFrom stats cov.wt model.frame predict
#' @import plyr
pseudoR2.lmerMod <- function(model){
  efLos = ranef(model)
  ret = c(sapply(VarCorr(model), function(x) x[[1]]),
          attributes(VarCorr(model))$sc^2)

  dt = data.frame(pred = predict(model, re.form = ~0),
                  efekt = model.frame(model)[, names(efLos)])
  pred = NULL # aby usunąć komunikat 'note' z check.
  tab = ddply(dt, ~efekt, summarise, mean = mean(pred),
              var = as.numeric(cov.wt(as.matrix(pred), method = "ML")$cov),
              n = length(pred))
  # tab = ddply(dt, ~efekt, summarise, mean=mean(pred), var=biasedVar(pred), n=length(pred))

  EEf = sum(tab$mean*tab$n)/sum(tab$n)
  varE = sum((tab$mean - EEf)^2*tab$n)/sum(tab$n)

  Evar = sum(tab$var*tab$n)/sum(tab$n)
  ret = cbind(ret, c(varE, Evar ))
  ret = cbind(ret, apply(ret, 1, sum))
  ret = rbind(ret, apply(ret, 2, sum))
  rownames(ret) <- c(names(efLos), "Resid.", "Suma")
  colnames(ret) <- c("Ef. Los", "Ef. St.", "Suma")
  return(ret)
}

fixef.lm <- function(model){
  return(coef(model))
}

ranef.lm <- function(model){
  return(list())
}

#' @title Mapowanie zmiennych dla modelu z interakcjami
#' @description
#' Celem funkcji jest stworzenie tablicy, która zawiera mapowanie kolumn ramki
#' danych, na której był budowany model na nazwy współczynników modelu
#' (i macierzy modelu).
#' @param model parametr klasy 'lmerMod' lub 'lm'.
#' @param zmiennaWielomianowa nazwa zmiennej, która jest parametrem wielomianu.
#' @return funkcja zwraca macierz logiczną, której nazwy wierszy to zmienne
#' ramki danych, a nazwy kolumn to nazwy współczynników modelu
#' @importFrom stats model.frame model.matrix
model_map = function(model, zmiennaWielomianowa){
  # macierz zawierająca przypisanie zmiennych z formuły do efektów
  # poszczególnych rzędów, ale jeszcze bez rozbicia factorów na dummiesy
  mapowanie = attributes(attributes(model.frame(model))$terms)$factors
  # wykluczamy z niej zmienne definiujące efekty losowe - najpierw wiersze
  mapowanie = mapowanie[!(rownames(mapowanie) %in% names(ranef(model))), ]
  # a następnie kolumny, które były z nimi powiązane
  mapowanie = mapowanie[, colSums(mapowanie) > 0]
  # maska wskazująca na wiersze powiązane ze zmienną wprowadzaną wielomianem
  # (gdy w formule jest poly(), wiersz jest tylko jeden, ale gdy wielomian był
  # wprowadzany przez zm + I(zm^2) + ..., to jest ich wiele)
  maskaZmWielomian = grepl(paste0("^(|poly[(]|I[(])", zmiennaWielomianowa,
                                  "(|,.+[)]|[ ^][[:digit:]]+[)])$"),
                           rownames(mapowanie))
  # i te ew. wiele wierszy zamieniamy na jeden
  mapowanie = rbind(mapowanie[!maskaZmWielomian, , drop = FALSE],
                    zmWielomian = as.numeric(colSums(mapowanie[maskaZmWielomian, ,
                                                               drop = FALSE]) > 0))
  rownames(mapowanie)[nrow(mapowanie)] = zmiennaWielomianowa
  # dodajemy jeszcze kolumnę na stałą regresji
  mapowanie = cbind(0, mapowanie)
  # i korzystamy z faktu, że w pewnym tajemniczym miejscu zapisane jest
  # mapowanie kolumn model.matrix na kolumny tak powstałej macierzy
  mapowanie = mapowanie[, 1 + attributes(model.matrix(model))$assign]
  # na koniec jeszcze przypisujemy nazwy kolumn z model.matrix
  colnames(mapowanie) = colnames(model.matrix(model))
  # i żeby wygodniej było tego dalej używać, przerzucamy na macierz wartości logicznych
  mapowanie = mapowanie == 1
  return(mapowanie)
}
