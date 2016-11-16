#' @title Kalkulator EWD
#' @description
#' Funkcja zwraca listę zawierającą zestawienia, które trzeba wkleić do
#' odpowiedniego arkusza kalkulacyjnego i przesłać do Blue Bricka, aby mogli
#'  zaimplementować modele w Kalkulatorze.
#' @param modele lista modeli regresji MNK
#' @param normyWe lista zawierająca mapowania wyników surowych egzaminów
#' na wejściu na skalę 100;15
#' @param normyWy lista zawierająca mapowania wyników surowych egzaminów
#' na wyjściu na skalę 100;15
#' @param dane data frame z danymi używanymi do estymacji ww. modeli
#' @details
#' Dane muszą być przekazane oddzielnym argumentem, aby uprościć sobie sprawę
#' z wykrywaniem, jakie były najniższe wartości egzaminu na wejściu występujące
#' w danych (jeśli w formule modelu użyto funkcji \code{poly}, jest to
#' niebanalne).
#' @return lista
#' @importFrom stats coef density formula model.frame predict resid setNames
#' @export
dane_do_kalkulatora = function(modele, normyWe, normyWy, dane) {
  stopifnot(is.list(modele), is.list(normyWe), is.list(normyWy))

  normyWe = lapply(normyWe, function(x) {
    if (all(c("wartosc", "wartosc_zr", "konstrukt") %in% names(x))) {
      konstrukt = x$konstrukt[1]
      if ("grupa" %in% names(x)) {
        maskaZmienne = c("grupa", "wartosc", "wartosc_zr")
        nazwyZmiennych = c("grupa", "suma", konstrukt)
      } else {
        maskaZmienne = c("wartosc", "wartosc_zr")
        nazwyZmiennych = c("suma", konstrukt)
      }
      x = x[, maskaZmienne]
      names(x) = nazwyZmiennych
    }
    return(x)
  })
  normyWy = lapply(normyWy, function(x) {
    if (all(c("wartosc", "wartosc_zr", "konstrukt") %in% names(x))) {
      konstrukt = x$konstrukt[1]
      if ("grupa" %in% names(x)) {
        maskaZmienne = c("grupa", "wartosc", "wartosc_zr")
        nazwyZmiennych = c("grupa", "suma", konstrukt)
      } else {
        maskaZmienne = c("wartosc", "wartosc_zr")
        nazwyZmiennych = c("suma", konstrukt)
      }
      x = x[, maskaZmienne]
      names(x) = nazwyZmiennych
    }
    return(x)
  })
  # wyliczanie przewidywań
  przew = setNames(vector(mode = "list", length = length(modele)), names(modele))
  for (i in 1:length(modele)) {
    lata = as.numeric(gsub("[^[:digit:]]", "", names(normyWe)))
    temp = mapply(function(x, y) {return(cbind(x, wydl = y))},
                  normyWe,
                  as.list(max(lata) - lata),
                  SIMPLIFY = FALSE)
    names(temp) = max(lata) - lata
    temp =
      lapply(temp,
             function(x, dane, model) {
               maskaPoprawki = grep("^plec$|^dysl(|eksja)_",
                                    names(model.frame(model)))
               temp = model.frame(model)[1:nrow(x), maskaPoprawki]
               for (i in 1:ncol(temp)) {
                 temp[, i] = levels(temp[, i])[1]
               }
               x = cbind(x, temp)
               x$wydl = factor(x$wydl, levels = levels(model.frame(model)$wydl))
               wydl = x$wydl[1]
               skrotEgzWe = all.vars(formula(model))[-1]
               skrotEgzWe = setNames(skrotEgzWe, skrotEgzWe)
               skrotEgzWe = skrotEgzWe[!grepl("^(plec|wydl)$|^dysl(|eksja)_", skrotEgzWe)]
               skrotEgzWe = sub("^(sum|norm|irt)_|_(norm|suma|irt)$", "", skrotEgzWe)
               if (!any(skrotEgzWe %in% names(x))) {
                 return(NULL)
               }
               names(x) = sub(paste0("^", skrotEgzWe, "$"), names(skrotEgzWe), names(x))
               x = cbind(x[, names(x) %in% c("suma", names(skrotEgzWe))],
                         przew = predict(model, x))
               # ręczny tuning przewidywania dla wartości, które są większe/mniejsze
               # niż największa/najmniejsza wartość zm. nzal. zarejestrowana w danych
               ekstrema = range(dane[dane$wydl == wydl, names(skrotEgzWe)],
                                na.rm = TRUE)
               x$przew[x[, names(skrotEgzWe)] < ekstrema[1]] =
                 x$przew[x[, names(skrotEgzWe)] == ekstrema[1]]
               x$przew[x[, names(skrotEgzWe)] > ekstrema[2]] =
                 x$przew[x[, names(skrotEgzWe)] == ekstrema[2]]
               # zapis
               names(x)[names(x) != "suma"] = paste0(names(x)[names(x) != "suma"], "_", wydl)
               return(x)
             },
             dane = dane, model = modele[[i]])
    temp = temp[!unlist(lapply(temp, is.null))]
    temp = suppressMessages(join_all(temp, type = "full"))
    przew[[i]] = temp
  }
  # łączenie wyników z różnych modeli
  przew = mapply(
    function(x, y) {
      maska = grepl("przew_", names(x))
      names(x)[maska] = paste0(names(x)[maska], "_", y)
      return(x)
    },
    przew, as.list(names(przew)),
    SIMPLIFY = FALSE)
  przew = suppressMessages(join_all(przew, type = "full"))
  przew = przew[, c("suma",
                    names(przew)[!grepl("^suma$|^przew_", names(przew))],
                    names(przew)[ grepl("^przew_", names(przew))])]
  normyWy = suppressMessages(join_all(normyWy, type = "full"))
  # poprawki na płeć i dysleksję
  poprawki = mapply(
    function(x, y) {
      x = coef(x)
      x = x[grepl("^plec|^(dysl|dysleksja)_", names(x))]
      x = data.frame(param = names(x), wartosc = x)
      names(x) = sub("^wartosc$", paste0("wartosc_", y), names(x))
      rownames(x) = NULL
      return(x)
    },
    modele, as.list(names(modele)),
    SIMPLIFY = FALSE)
  poprawki = suppressMessages(join_all(poprawki, type = "full"))
  # rozkłady reszt
  reszty = lapply(modele, function(x) {
    return(as.data.frame(density(resid(x), adjust = 1.5, n = 1024,
                                 na.rm = TRUE)[c("x", "y")]))
  })
  # kończenie
  return(list(przew = przew,
              normyWy = normyWy,
              poprawki = poprawki,
              gestoscReszt = reszty))
}
