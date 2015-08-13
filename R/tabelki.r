#' @title Liczba zdających egzaminy.
#' @description
#' Funkcja zwraca tabelę z zestawieniem liczby podchodzących do dwóch egzaminów
#' (na wejściu i na wyjściu), z wyszczególnieniem liczby laureatów i dyslektyków.
#' @param x data frame z danymi potrzebnymi do przygotowania zestawienia
#' @param nazwyZmWynikiEgzWy wektor ciągów znaków zawierający nazwy zmiennych
#' z wynikami poszczególnych części egzaminu "na wyjściu", dla których ma być
#' przygotowane zestawienie
#' @param kodyCzesciEgzWe wektor ciągów znaków - kody części egzaminu
#' "na wejściu" (np. "gm_m", "s")
#' @param nazwaEgzWy ciąg znaków - nazwa egzaminu na wyjściu (do nagłówka tabeli)
#' @param nazwaEgzWe ciąg znaków - nazwa egzaminu na wejściu (do nagłówka tabeli)
#' @return data frame
#' @export
tabelka_ld = function(x, nazwyZmWynikiEgzWy, kodyCzesciEgzWe,
                      nazwaEgzWy = "egz. wy.", nazwaEgzWe = "egz. we.") {
  stopifnot(is.data.frame(x),
            is.character(nazwyZmWynikiEgzWy), length(nazwyZmWynikiEgzWy) > 0,
            is.character(kodyCzesciEgzWe), length(kodyCzesciEgzWe) > 0,
            is.character(nazwaEgzWy), length(nazwaEgzWy) == 1,
            is.character(nazwaEgzWe), length(nazwaEgzWe) == 1
  )
  stopifnot(all(nazwyZmWynikiEgzWy %in% names(x)),
            paste0("rok_", substr(kodyCzesciEgzWe, 1, 1)) %in% names(x),
            all(grepl("^(sum|norm|irt|rsch)_|_(suma|norm)$", nazwyZmWynikiEgzWy))
  )

  skrotEgzWe = unique(substr(kodyCzesciEgzWe, 1, 1))
  skrotEgzWy = sub("^(sum|norm|irt|rsch)_|_(suma|norm)$", "", nazwyZmWynikiEgzWy)
  skrotEgzWy = unique(substr(skrotEgzWy, 1, 1))
  lZmLaurWe = sum(grepl(paste0("^laureat_", skrotEgzWe), names(x)))

  # przygotowujemy macierz z nazwami wierszy i kolumn
  tabelkaLD =
    matrix(NA, nrow = 5 + lZmLaurWe, ncol = length(nazwyZmWynikiEgzWy),
           dimnames = list(
             c("ogółem",
               paste0("dysleksja ", nazwaEgzWy),
               paste0("dysleksja ", nazwaEgzWe),
               paste0("dysleksja ", nazwaEgzWe, " i ", nazwaEgzWy),
               paste0("laureaci ", nazwaEgzWy),
               paste0("laureaci ", kodyCzesciEgzWe)),
             nazwyZmWynikiEgzWy))
  # żeby ją wypełnić
  x = within(x, {
    dysleksja_temp = factor((get(paste0("dysleksja_", skrotEgzWy)) %in% "tak") &
      (get(paste0("dysleksja_", skrotEgzWe)) %in% "tak"), levels = c(FALSE, TRUE),
      labels = c("nie", "tak"))
  })
  for (i in nazwyZmWynikiEgzWy) {
    kodCzesci = sub("^(sum|norm|irt|rsch)_|_(suma|norm)$", "", i)
    skrotEgzWy = substr(kodCzesci, 1, 1)
    maskaBD = !is.na(x[, i])
    zmDoZsumowania = c(
      paste0("dysleksja_", skrotEgzWy),
      paste0("dysleksja_", skrotEgzWe),
      "dysleksja_temp",
      paste0("laureat_", kodCzesci),
      paste0("laureat_", kodyCzesciEgzWe)
    )
    temp = lapply(x[, zmDoZsumowania], function(x, maska) {
      return(sum(x %in% "tak" & maska))
    }, maska = maskaBD)
    tabelkaLD[, colnames(tabelkaLD) == i] = c(sum(maskaBD), unlist(temp))
  }
  tabelkaLD = data.frame("zdający" = rownames(tabelkaLD), tabelkaLD,
                         stringsAsFactors = FALSE)
  return(tabelkaLD)
}
#' @title Parametry rozkładów zmiennych.
#' @description
#' Funkcja zwraca wartości zestawu statystyk opisowych: kwartyle, średnia, odch.
#' stand. dla zestawu zmiennych, w podziale na grupy.
#' @param x data frame lub lista ze zmiennymi, dla których mają zostać wyliczone
#' statystyki
#' @param grBezLacznie wektor ciągów znaków lub NULL, gdy nie dotyczy - nazwy
#' zmiennych grupujących, dla których statystyki mają być zwrócone tylko
#' w podziale na grupy, ale bez podawania statystyk dla całej zbiorowości
#' @param grZLacznie ciąg znaków lub NULL, gdy nie dotyczy - nazwa zmiennej
#' grupującej (może być tylko jedna!), dla której statystyki mają być zwrócone
#' zarówno w podziale na grupy, jak i dla całej zbiorowości
#' @param nazwaPierwKol ciąg znaków - nazwa pierwszej kolumny zwracenego data
#' frame'a, zawierającej nazwy zmiennych, dla których wyliczono statystyki
#' @details
#' W \code{x} muszą znajdować się zarówno zmienne, dla których wyliczona mają
#' zostać statystyki, jak i zmienne grupujące, których nazwy podają parametry
#' \code{grBezLacznie} i \code{grZLacznie}.
#' Jeśli oba argumenty \code{grBezLacznie} i \code{grZLacznie} został podane,
#' dzielenia na grupy ze względu na \code{grZLacznie} zostanie przeprowadzone
#' w ramach grup wyróżnionych ze względu na \code{grBezLacznie}.
#' @return data frame
#' @import plyr
#' @export
parametry_egz = function(x, grBezLacznie, grZLacznie, nazwaPierwKol = NA) {
  stopifnot(is.data.frame(x) | is.list(x),
            is.character(grBezLacznie ) | is.null(grBezLacznie ),
            is.character(grZLacznie   ) | is.null(grZLacznie   ),
            is.character(nazwaPierwKol) | all(is.na(nazwaPierwKol))
  )
  stopifnot(grBezLacznie %in% names(x),
            grZLacznie   %in% names(x)
  )
  if (!is.null(grBezLacznie)) {
    stopifnot(length(grBezLacznie) > 0, all(grBezLacznie != ""))
  } else {
    grBezLacznie = "grBezLacznie"
    x = cbind(x, grBezLacznie = 1)
  }
  if (!is.null(grZLacznie  )) {
    stopifnot(length(grZLacznie) == 1, grZLacznie != "")
  } else {
    grZLacznie = "grZLacznie"
    x = cbind(x, grZLacznie = 1)
  }
  x = ldply(as.list(x[, !(names(x) %in% c(grBezLacznie, grZLacznie) ), drop = FALSE]),
            function(x, grBezLacznie, grZLacznie) {
              ddply(data.frame(x, grBezLacznie, grZLacznie), names(grBezLacznie),
                    function(x, grBezLacznie) {
                      maska = !(names(x) %in% c(grBezLacznie, "grZLacznie"))
                      tempOg = data.frame(grZLacznie = "łącznie",
                                          as.list(moje_parametry(x[, maska])),
                                          check.names = FALSE)
                      if (length(unique(x$grZLacznie)) > 1) {
                        tempGr = ddply(x, "grZLacznie",
                                       function(x) {
                                         maska = !(names(x) %in% c(grBezLacznie,
                                                                   "grZLacznie"))
                                         return(moje_parametry(x[, maska]))
                                       })
                        return(rbind.fill(list(tempOg, tempGr)))
                      } else {
                        return(tempOg)
                      }
                    }, grBezLacznie = names(grBezLacznie))
            }, grBezLacznie = x[, grBezLacznie, drop = FALSE], grZLacznie = x[, grZLacznie],
            .id = nazwaPierwKol)
  if (length(unique(x$grBezLacznie)) == 1) {
    x = x[, names(x) != "grBezLacznie"]
  }
  if (length(unique(x$grZLacznie  )) == 1) {
    x = x[, names(x) != "grZLacznie"  ]
  }
  names(x) = sub("grZLacznie"  , grZLacznie  , names(x))
  return(x)
}
#' @title Parametry rozkładów zmiennych.
#' @description
#' Funkcja zwraca wartości zestawu statystyk opisowych: kwartyle, średnia, odch. stand.
#' @param x wektor liczbowy
#' @param na.rm wartość logiczna - przekazywana do funkcji wyliczających statystyki
#' @param digits liczba całkowita - do ilu miejsc po przecinku zaokrąglać zwracane wyniki
#' @return wektor liczb
moje_parametry = function(x, na.rm = TRUE, digits = 2) {
  stopifnot(is.numeric(x), is.logical(na.rm), is.numeric(digits),
            length(na.rm) == 1, length(digits) == 1)
  stopifnot(na.rm %in% c(TRUE, FALSE))
  return(round(
    c(
      "min"        = min(x, na.rm = na.rm),
      "1. kw."     = as.numeric(quantile(x, prob = 0.25, na.rm = na.rm)),  #as.numeric, żeby pozbyć się nazwy
      "mediana"    = median(x, na.rm = na.rm),
      "3. kw."     = as.numeric(quantile(x, prob = 0.75, na.rm = na.rm)),
      "max"        = max(x, na.rm = na.rm),
      "średnia"    = mean(x, na.rm = na.rm),
      "odch. std." = sd(x, na.rm = na.rm)
    ), digits))
}
