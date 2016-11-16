#' @title Wyliczanie EWD
#' @description
#' Funkcja przekształca skale zmiennych z surowymi lub wyskalowanymi wynikami
#' egzaminów ze 100;15 na 0;1, lub odwrotnie, adkewatnie do tego, jak typowo
#' są one wykorzystywane w modelach EWD.
#' @param dane data frame z danymi, typowo wczytany do R wynik działania
#' funkcji \code{\link[EWDdane]{przygotuj_dane_do_ewd}}
#' @param czesciEgzWy wektor ciągów znaków - kody konstruktów, które będą
#' w modelach EWD zmiennymi zależnymi
#' @param sufiksWyniki ciąg znaków: "irt", "norm" lub "suma"; określa, które
#' wyniki będą używane do wyliczania wskaźników i mają zostać przekształcone
#' @return data frame
#' @export
przeksztalc_skale_zmiennych = function(dane, czesciEgzWy, sufiksWyniki) {
  stopifnot(is.data.frame(dane),
            is.character(czesciEgzWy), length(czesciEgzWy) > 0,
            is.character(sufiksWyniki), length(sufiksWyniki) == 1)
  stopifnot(sufiksWyniki %in% c("irt", "norm", "suma"))

  if (sufiksWyniki == "irt") {
    # jeśli operujemy na wynikach wyskalowanych IRT, to zm. zależne trzeba przerzucić na 100;15
    for (i in intersect(names(dane), paste0(czesciEgzWy, "_irt"))) {
# brut-hack na problem z nietrzymaniem skali przez zapisane w bazie oszacowania umiejętności z matury
#       for (j in unique(dane$rok_m)) {
#         maskaRok = dane$rok_m == j
#         maska = maskaRok & !dane$pomin_szkole
#         sr = mean(dane[maska, i], na.rm = TRUE)
#         os = sd(dane[maska, i], na.rm = TRUE)
#         dane[maskaRok, i] = (dane[maskaRok, i] - sr) / os
#         dane[maskaRok, i] = 100 + 15 * dane[maskaRok, i]
#       }
      dane[, i] = 100 + 15 * dane[, i]
    }
  } else if (sufiksWyniki == "norm") {
    # jeśli operujemy na wynikach znormalizowanych, to zm. nzal. warto przerzucić na 0;1
    for (i in setdiff(names(dane)[grepl("_norm$", names(dane)) &
                                  !grepl("^bs_", names(dane))],
                      intersect(names(dane), paste0(czesciEgzWy, "_norm")))) {
      dane[, i] = (dane[, i] - 100) / 15
    }
  }
  return(dane)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja przekształca wyniki egzaminów wyrażone na skalach raschowych
#' na podstawie podanych norm. Typowo na potrzeby jednorocznych modeli
#' maturalnych.
#' @param dane data frame z danymi, typowo wczytany do R wynik działania
#' funkcji \code{\link[EWDdane]{przygotuj_dane_do_ewd}}
#' @param normy lista data frame'ów z normami, typowo wynik działania funkcji
#' \code{\link{przeksztalc_skale_norm}}
#' @details
#' W danych muszą znajdować się zmienne opisujące przydział obserwacji do grup
#' (o nazwach postaci \code{grupa_s}, gdzie 's' to kod rodzaju egzaminu:
#' 's' - sprawdzian, 'g' - egz. gimn., 'm' - matura) i surowe wyniki skal
#' (o nazwach postaci \code{konstrukt_suma}, gdzie 'konstruktR' to nazwa
#' konstruktu estymowanego w ramach skali - modelem raschowym). Jeśli w danych
#' znajdują się już zmienne opisujące wyniki wyskalowane (o nazwach postaci
#' \code{konstrukt_irt}), zostaną one zastąpione.
#' @return data frame
#' @import plyr
#' @export
przeksztalc_dane_normami = function(dane, normy) {
  stopifnot(is.data.frame(dane), is.list(normy))

  # łączenie norm dla tych samych konstruktów z różnych lat
  names(normy) = sub("_[[:digit:]]+$", "", names(normy))
  konstrukty = unique(names(normy))
  for (i in konstrukty) {
    normyTemp = rbind.fill(normy[names(normy) == i])
    normy = normy[names(normy) != i]
    normy[[length(normy) + 1]] = normyTemp
    names(normy)[length(normy)] = i
  }

  # samo przekształcanie wartosci
  for (i in 1:length(normy)) {
    konstrukt = unique(normy[[i]]$konstrukt)
    stopifnot(length(konstrukt) == 1)
    skrotEgz = substr(konstrukt, 1, 1)
    stopifnot(skrotEgz %in% c("s", "g", "m"))

    normyTemp = normy[[i]][, c("grupa", "rok", "wartosc", "wartosc_zr")]
    names(normyTemp) = c(paste0("grupa_", skrotEgz), paste0("rok_", skrotEgz),
                         paste0(konstrukt, "_suma"), paste0(konstrukt, "_irt"))
    stopifnot(all(names(normyTemp) %in% names(dane)))
    dane = dane[, names(dane) != paste0(konstrukt, "_irt")]
    dane = suppressMessages(join(dane, normyTemp))
  }
  return(dane)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja przekształca skale norm zgodnie z podanymi parametrami. Typowo na
#' potrzeby jednorocznych modeli maturalnych, gdzie zachodzi ten problem, że
#' wyniki egzaminu gimnazjalnego nie są wystandaryzowane względem poszczególnych
#' typów szkół ponadgimnazjalnych.
#' @param normy lista data frame'ów z normami, typowo atrybut \code{normy}
#' wczytanego do R wynik działania funkcji
#' \code{\link[EWDdane]{przygotuj_dane_do_ewd}}
#' @param parametry data frame z parametrami (średnia, odchylenie standardowe)
#' zmiennych do przekształcenia, typowo wynik działania funkcji
#' \code{\link{parametry_rozkladu_rasch}}
#' @param na10015 wartość logiczna - czy po przekształceniu zgodnie z podanymi
#' parametrami przemnożyć wartości wszystkich norm przez 15 i dodać 100?
#' (tj. przekształcić na skalę 100;15, zakładając, że po ew. przekształceniach
#' są wystandaryzowane do średniej 0 i odch. std. 1)
#' @return lista o strukturze zgodnej ze strukturą argumentu \code{normy}
#' @export
przeksztalc_skale_norm = function(normy, parametry, na10015 = TRUE) {
  stopifnot(is.list(normy), is.data.frame(parametry),
            is.logical(na10015), length(na10015) == 1)
  stopifnot(all(c("rok", "zmienna", "srednia", "odchStd") %in% names(parametry)),
            na10015 %in% c(TRUE, FALSE))

  for (i in 1:nrow(parametry)) {
    nazwa = paste0(parametry$zmienna[i], "_", parametry$rok[i])
    if (is.null(normy[[nazwa]])) {
      warning("Nie znaleziono norm opisanych jako'", nazwa, "'.",
              immediate. = TRUE)
    }
    normy[[nazwa]]$wartosc_zr =
      (normy[[nazwa]]$wartosc_zr - parametry$srednia[i]) / parametry$odchStd[i]
  }
  if (na10015) {
    normy = lapply(normy, function(x) {
      x$wartosc_zr = 100 + (x$wartosc_zr * 15)
      return(x)
    })
  }
  return(normy)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja wylicza średnie i odchylenia standardowe zmiennych z wynikami
#' egzaminóW wyskalowanymi modelami rascha. Typowo na potrzeby jednorocznych
#' modeli maturalnych, gdzie zachodzi ten problem, że wyniki egzaminu
#' gimnazjalnego nie są wystandaryzowane względem poszczególnych typów szkół
#' ponadgimnazjalnych.
#'
#' Pod uwagę brani są przy tym jedynie uczniowie o standardowej długości toku
#' kształcenia.
#' @param dane data frame z danymi, typowo wczytany do R wynik działania
#' funkcji \code{\link[EWDdane]{przygotuj_dane_do_ewd}}
#' @param czesciEgzWy wektor ciągów znaków, pozwala podać, zmienne związane
#' z jakimi konstruktami mają zostać pominięte
#' @return data frame
#' @importFrom stats sd
#' @export
parametry_rozkladu_rasch = function(dane, czesciEgzWy = "") {
  stopifnot(is.data.frame(dane),
            is.character(czesciEgzWy))
  stopifnot(all(c("pomin_szkole", "wydl") %in% names(dane)))

  dane = subset(dane, !get("pomin_szkole") & get("wydl") == "0")
  maska = names(dane)[grep(paste0("R_irt$"), names(dane))]
  maska = maska[!grepl(paste0("^(bs|grupa|",
                              paste0(czesciEgzWy, collapse = "|"),
                              ")_"), maska)]
  skrotEgz = unique(substr(maska, 1, 1))
  stopifnot(length(skrotEgz) == 1)
  zmRok = paste0("rok_", skrotEgz)
  stopifnot(length(unique(dane[[zmRok]])) == 1)
  return(data.frame(rok = dane[[zmRok]][1],
                    zmienna = sub("_irt$", "", maska),
                    srednia = unlist(lapply(dane[maska], mean, na.rm = TRUE)),
                    odchStd = unlist(lapply(dane[maska], sd, na.rm = TRUE)),
                    stringsAsFactors = FALSE))
}

