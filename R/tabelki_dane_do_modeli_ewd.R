#' @title Liczba zdających egzaminy.
#' @description
#' Funkcja przygotowuje zestaw tabelek z zestawieniem liczby podchodzących do
#' egzaminów i statystykami opisowymi wyników.
#' @param x data frame z danymi potrzebnymi do przygotowania zestawienia
#' @param czesciEgzWy wektor ciągów znaków - kody części egzaminu "wyjściu"
#' (np. "gh_h")
#' @param czesciEgzWe - wektor ciągów znaków - kody części egzaminu "wejściu"
#' (np. "s")
#' @return lista data frame'ów
#' @importFrom stats na.omit
#' @export
tabelki_dane_do_modeli_ewd = function(x, czesciEgzWy, czesciEgzWe) {
  stopifnot(is.data.frame(x),
            is.character(czesciEgzWy), length(czesciEgzWy) > 0,
            is.character(czesciEgzWe), length(czesciEgzWe) > 0)

  skrotEgzWy = unique(substr(czesciEgzWy, 1, 1))
  skrotEgzWe = unique(substr(czesciEgzWe, 1, 1))

  nazwyZmSumy = names(x)[grep("^sum_|_suma$", names(x))]
  nazwyZmNorm = names(x)[grep("^(norm|irt)_|_(norm|irt)$", names(x))]
  nazwySumyEgzWy =
    nazwyZmSumy[sub("^sum_|_suma$", "", nazwyZmSumy) %in% czesciEgzWy]
  nazwyNormEgzWy =
    nazwyZmNorm[sub("^(norm|irt)_|_(norm|irt)$", "", nazwyZmNorm) %in% czesciEgzWy]
  nazwySumyEgzWe =
    nazwyZmSumy[sub("^sum_|_suma$", "", nazwyZmSumy) %in% czesciEgzWe]
  nazwyNormEgzWe =
    nazwyZmNorm[sub("^(norm|irt)_|_(norm|irt)$", "", nazwyZmNorm) %in% czesciEgzWe]
  if (length(nazwySumyEgzWy) > 0) {
    nazwyDoLZWy = nazwySumyEgzWy
  } else {
    nazwyDoLZWy = nazwyNormEgzWy
  }
  if (length(nazwySumyEgzWe) > 0) {
    nazwyDoLZWe = nazwySumyEgzWe
  } else {
    nazwyDoLZWe = nazwyNormEgzWe
  }

  zmRokWy = paste0("rok_", skrotEgzWy)
  zmRokWe = paste0("rok_", skrotEgzWe)
  lataWy = length(unique(na.omit(x[, zmRokWy])))
  if (lataWy > 1) {
    grBezLacznie1 = zmRokWy
    grBezLacznie2 = c(grBezLacznie1, zmRokWe)
    zmRok1 = c(zmRokWy)
    zmRok2 = c(zmRokWy, zmRokWe)
  } else {
    grBezLacznie1 = NULL
    grBezLacznie2 = zmRokWe
    zmRok1 = NULL
    zmRok2 = zmRokWe
  }

  mapaNazwEgz = list("s" = "spr.", "g" = "egz. gimn.", "m" = "matura")
  nazwaEgzWe = mapaNazwEgz[[skrotEgzWe]]
  nazwaEgzWy = mapaNazwEgz[[skrotEgzWy]]

  wyniki = list(
    liczbaZdajacych = tabelka_ld(x, nazwyZmWynikiEgzWy = nazwyDoLZWy,
                                 kodyCzesciEgzWe = czesciEgzWe,
                                 nazwaEgzWy = nazwaEgzWy,
                                 nazwaEgzWe = nazwaEgzWe),
    liczbaZdajacychPoLatach = ddply(x, c(zmRokWy, zmRokWe),
                                    tabelka_ld,
                                    nazwyZmWynikiEgzWy = nazwyDoLZWy,
                                    kodyCzesciEgzWe = czesciEgzWe,
                                    nazwaEgzWy = nazwaEgzWy,
                                    nazwaEgzWe = nazwaEgzWe)
  )
  if (length(nazwySumyEgzWy) > 0) {
    wyniki$wySumPlec = parametry_egz(x[, c(nazwySumyEgzWy, "plec", zmRok1)],
                                     grBezLacznie = grBezLacznie1,
                                     grZLacznie = "plec",
                                     nazwaPierwKol = "zmienna")
    wyniki$wySumWydl = parametry_egz(x[, c(nazwySumyEgzWy, zmRok2)],
                                     grBezLacznie = grBezLacznie2,
                                     grZLacznie = NULL,
                                     nazwaPierwKol = "zmienna")
    wyniki$weSumPlec = parametry_egz(x[, c(nazwySumyEgzWe, zmRok2, "plec")],
                                     grBezLacznie = grBezLacznie2,
                                     grZLacznie = "plec",
                                     nazwaPierwKol = "zmienna")
  }
  if (length(nazwyNormEgzWy) > 0) {
    wyniki$wyNormPlec = parametry_egz(x[, c(nazwyNormEgzWy, "plec", zmRok1)],
                                      grBezLacznie = grBezLacznie1,
                                      grZLacznie = "plec",
                                      nazwaPierwKol = "zmienna")
    wyniki$wyNormWydl = parametry_egz(x[, c(nazwyNormEgzWy, zmRok2)],
                                      grBezLacznie = grBezLacznie2,
                                      grZLacznie = NULL,
                                      nazwaPierwKol = "zmienna")
    wyniki$weNormPlec = parametry_egz(x[, c(nazwyNormEgzWe, zmRok2, "plec")],
                                      grBezLacznie = grBezLacznie2,
                                      grZLacznie = "plec",
                                      nazwaPierwKol = "zmienna")
  }

  for (i in 1:length(wyniki)) {
    if (zmRokWe %in% names(wyniki[[i]])) {
      wyniki[[i]] = wyniki[[i]][order(-wyniki[[i]][, zmRokWe]), ]
    }
    if (zmRokWy %in% names(wyniki[[i]])) {
      wyniki[[i]] = wyniki[[i]][order(-wyniki[[i]][, zmRokWy]), ]
    }
    if ("zmienna" %in% names(wyniki[[i]])) {
      wyniki[[i]] = wyniki[[i]][order(wyniki[[i]]$zmienna), ]
    }
  }
  cat("Liczba obserwacji uwzględnionych w analizach\n")
  print(wyniki$liczbaZdajacych, row.names = FALSE)
  cat("\nLiczba obserwacji uwzględnionych w analizach w podziale na lata\n")
  print(wyniki$liczbaZdajacychPoLatach, row.names = FALSE)
  if (!is.null(wyniki$wySumPlec)) {
    cat("\nParametry łącznych wyników surowych egzaminów\n")
    print(wyniki$wySumPlec, row.names = FALSE)
    cat("\n")
    print(wyniki$wySumWydl, row.names = FALSE)
    cat("\n")
    print(wyniki$weSumPlec, row.names = FALSE)
  }
  if (!is.null(wyniki$wyNormPlec)) {
    cat("\nParametry łącznych wyników znormalizowanych egzaminów\n")
    print(wyniki$wyNormPlec, row.names = FALSE)
    cat("\n")
    print(wyniki$wyNormWydl, row.names = FALSE)
    cat("\n")
    print(wyniki$weNormPlec, row.names = FALSE)
  }

  return(wyniki)
}
