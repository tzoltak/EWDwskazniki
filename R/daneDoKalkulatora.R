#' @title Kalkulator EWD
#' @description
#' Funkcja zwraca listę zawierającą zestawienia, które trzeba wkleić do odpowiedniego
#' arkusza kalkulacyjnego i przesłać do Blue Bricka, aby mogli zaimplementować modele
#' w Kalkulatorze.
#' @param modele lista modeli regresji MNK
#' @param mapowanie lista zawierająca mapowania wyników surowych egzaminów na skalę 100;15
#' @param dane data frame z danymi używanymi do estymacji ww. modeli
#' @details
#' Dane muszą być przekazane oddzielnym argumentem, aby uprościć sobie sprawę
#' z wykrywaniem, jakie były najniższe wartości egzaminu na wejściu występujące w danych
#' (jeśli w formule modelu użyto funkcji \code{poly()}, jest to niebanalne).
#' @return lista
#' @export
dane_do_kalkulatora = function(modele, mapowanie, dane) {
  stopifnot(is.list(modele), is.list(mapowanie), is.list(mapowanie))

  # przeliczenie egz. na wejściu na 100;15
  przew = setNames(vector(mode="list", length=length(modele)), names(modele))
  for (i in 1:length(modele)) {
    maska = grep("^gm_r_", names(mapowanie))
    lata = as.numeric(gsub("[^[:digit:]]", "", names(mapowanie)[maska]))
    temp = mapply(function(x, y) {return(cbind(x, wydl=y))},
                  mapowanie[maska],
                  as.list(lata),
                  SIMPLIFY = FALSE)
    rokEgStd = max(lata)
    names(temp) = rokEgStd - lata
    temp = lapply(temp,
                  function(x, dane, model) {
                    x= cbind(x,
                             plec = factor("kobieta", levels=levels(model.frame(model)$plec)),
                             dysleksja_m = factor("nie", levels=levels(model.frame(model)$dysleksja_m)),
                             dysleksja_g = factor("nie", levels=levels(model.frame(model)$dysleksja_g)))
                    x$wydl = factor(max(dane$rok_g, na.rm=TRUE) - x$wydl, levels=levels(model.frame(model)$wydl))
                    names(x) = sub("^gm_r$", "gm", names(x))
                    wydl = x$wydl[1]
                    zmNzal = c("gm")
                    zmNzal = zmNzal[zmNzal %in% all.vars(formula(model))]
                    x = cbind(x[, names(x) %in% c("suma", zmNzal)], przew = predict(model, x))
                    # ręczny tuning przewidywania dla wartości, które są większe/mniejsze niż największa/najmniejsza wartość zm. nzal. zarejestrowana w danych
                    ekstrema = range(dane[dane$wydl == wydl, zmNzal], na.rm=TRUE)
                    x$przew[x[, zmNzal] < ekstrema[1]] = x$przew[x[, zmNzal] == ekstrema[1]]
                    x$przew[x[, zmNzal] > ekstrema[2]] = x$przew[x[, zmNzal] == ekstrema[2]]
                    # zapis
                    names(x)[names(x) != "suma"] = paste0(names(x)[names(x) != "suma"], "_", wydl)
                    return(x)
                  },
                  dane = dane, model = modele[[i]])
    temp = suppressMessages(join_all(temp, type="full"))
    przew[[i]] = temp
  }
  # łączenie wyników z różnych modeli
  przew = mapply(
    function(x, y) {
      names(x)[names(x) != "suma"] = paste0(names(x)[names(x) != "suma"], "_", y)
      return(x)
    },
    przew, as.list(names(przew)),
    SIMPLIFY = FALSE)
  przew = suppressMessages(join_all(przew, type="full"))
  przew = przew[, c("suma",
                    names(przew)[!grepl("^suma$|^przew_", names(przew))],
                    names(przew)[ grepl("^przew_", names(przew))])]
  # przeliczenie egz. na wyjściu na 100;15
  mapowanieWy = mapowanie[czesciMatury]
  mapowanieWy = lapply(mapowanieWy,
                       function(x) {
                         x = dlply(x, names(x)[grep("^suma_", names(x))],
                                   function(x) {
                                     maska = grepl("^suma_", names(x))
                                     sufiks = paste0(as.numeric(x[1, maska]), collapse="")
                                     x = x[, !maska]
                                     names(x)[names(x) != "suma"] = paste0(names(x)[names(x) != "suma"],
                                                                           "_", sufiks)
                                     return(x)
                                   })
                         x = suppressMessages(join_all(x, type="full"))
                       })
  # łączenie wyników z różnych modeli
  mapowanieWy = suppressMessages(join_all(mapowanieWy, type="full"))
  # poprawki na płeć i dysleksję
  poprawki = mapply(
    function(x, y) {
      x = coef(x)
      x = x[grepl("^(plec|dysleksja)", names(x))]
      x = data.frame(param = names(x), wartosc=x)
      names(x) = sub("^wartosc$", paste0("wartosc_", y), names(x))
      rownames(x) = NULL
      return(x)
    },
    modele, as.list(names(modele)),
    SIMPLIFY = FALSE)
  poprawki = suppressMessages(join_all(poprawki, type="full"))
  # rozkłady reszt
  reszty = lapply(modele, function(x) {
    return(as.data.frame(density(resid(x), adjust=1.5, n=1024, na.rm=TRUE)[c("x", "y")]))
  })
  # kończenie
  return(list(przew = przew,
              mapowanieWy = mapowanieWy,
              poprawki = poprawki,
              gestoscReszt = reszty))
}
