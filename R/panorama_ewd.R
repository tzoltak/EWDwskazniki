#' @title Diagnostyka wskaźników EWD
#' @description
#' Funkcja rysuje "panoramę polskiej edukacji", czyli wykres rozrzutu szkół ze
#' względu na wartości średnich wyników "na wejściu"/"na wyjściu" (oś X)
#' i wartości wskaźników EWD (oś Y).
#' @param x lista data frame'ów zwracana przez funkcję
#' \code{\link{przygotuj_wsk_ewd}}
#' @param lata ciąg znaków opisujący, jakich lat wskaźnik dotyczy
#' @param katalogZapis opcjonalnie ciąg znaków podający nazwę katalogu, do
#' którego mają zostać zapisane rysunki (jeśli nie zostanie podana, rysunki
#' nie zostaną zapisane)
#' @param wybierzSzkoly opcjonalnie wyrażenie (expression), które zostanie
#' użyte do zawężenia grupy szkół, których wyniki będą prezentowane; np.
#' \code{expression(lu_wszyscy >= 10)}
#' @param typSzkol opcjonalnie ciąg znaków z nazwą typu szkół (potrzebny tylko
#' w przypadku matury)
#' @return funkcja nic nie zwraca.
#' @export
panorama_z_listy_wskaznikow = function(x, lata, katalogZapis = NULL,
                                       wybierzSzkoly = NULL,
                                       typSzkol = NULL) {
 stopifnot(
   "listaWskaznikowEWD" %in% class(x),
   is.null(katalogZapis) | is.character(katalogZapis),
   is.null(wybierzSzkoly) | is.expression(wybierzSzkoly),
   is.null(typSzkol) | is.character(typSzkol))
  if (!is.null(katalogZapis)) {
    stopifnot(length(katalogZapis) == 1)
  }
  if (!is.null(typSzkol)) {
    stopifnot(length(typSzkol) == 1)
    stopifnot(typSzkol %in% c("SP", "gimn.", "LO", "T"))
  }

  skrotEgzWy = unique(substr(names(x), 1, 1))
  dopelniaczEgz = list(
    "s" = "sprawdzianu",
    "g" = "egzaminu gimnazjalnego",
    "m" = "matury"
  )
  skrotEgzWe = names(dopelniaczEgz)[grep(skrotEgzWy, names(dopelniaczEgz)) - 1]
  dopelniaczEgzWy = dopelniaczEgz[[skrotEgzWy]]
  dopelniaczEgzWe = dopelniaczEgz[[grep(skrotEgzWy, names(dopelniaczEgz)) - 1]]
  if (is.null(typSzkol)) {
    typSzkol = list(
      "s" = "SP",
      "g" = "gimn.",
      "m" = "LO lub T"
    )[[skrotEgzWy]]
  }

  for (i in 1:length(x)) {
    if (!is.null(wybierzSzkoly)) {
      temp = subset(x[[i]], eval(wybierzSzkoly, x[[i]]))
    } else {
      temp = x[[i]]
    }
    # na X egz. "wyjściu"
    message(names(x)[i], " x ", paste0("ewd_", names(x)[i]))
    panorama_ewd(temp[, names(x)[i]], temp[, paste0("ewd_", names(x)[i])],
                 dopelniaczEgzWy, typSzkol, names(x)[i], lata, katalogZapis,
                 prElips = c(0.5, 0.9), lu = temp$lu_wszyscy, kolory = rep(1, 2))
    # na X egz. "wejściu"
    message("sr_we x ", paste0("ewd_", names(x)[i]))
    panorama_ewd(temp$sr_we, temp[, paste0("ewd_", names(x)[i])],
                 dopelniaczEgzWe, typSzkol, names(x)[i], lata, katalogZapis,
                 skrotEgzWe, prElips = c(0.5, 0.9),
                 lu = temp$lu_wszyscy, kolory = rep(1, 2))
  }
}

#' @title Diagnostyka wskaźników EWD
#' @description
#' Funkcja rysuje "panoramę polskiej edukacji", czyli wykres rozrzutu szkół ze
#' względu na wartości średnich wyników na wyjściu (oś X) i wartości wskaźników
#' EWD (oś Y).
#' @param sr wektor liczbowy z wartościami średnich wyników egzaminu na wyjściu
#' poszczególnych szkół
#' @param ewd wektor liczbowy z wartościami wskaźników EWD szkół
#' @param egzamin ciąg znaków z nazwą egzaminu (w dopełniaczu)
#' @param typ_szkoly ciąg znaków z typem szkoły (typowo "gimn.", "LO" lub "T")
#' @param wskaznik ciąg znaków z nazwą wskaźnika EWD
#' @param lata ciąg znaków opisujący, jakich lat wskaźnik dotyczy
#' @param zapiszPng ciąg znaków - ścieżka do katalogu, w którym mają być
#' zapisane pliki PNG z wykresami lub NULL, jeśli wykresy mają nie być zapisywane
#' @param xlab opcjonalnie ciąg znaków - podpis osi X, jeśli ma być inny niż
#' \code{wskaznik}
#' @param paleta funkcja definiująca paletę kolorów do wykorzystania przy rysowaniu
#' wykresów, typowo wynik wywołania funkcji \code{\link[grDevices]{colorRampPalette}}
#' @param prElips wektor liczb z przedziału (0;1), które oznaczają  odsetek szkół,
#' które mają się znaleźć w elipsie.
#' @param lu wektor liczbowy zawierający liczbę uczniów w poszczególnych
#' szkołach. Wymagany do wyznaczenia elips.
#' @param kolory wektor zawierający informację o kolorach elips. Domyślnie
#' elipsy są niebieskie.
#' @return funkcja nic nie zwraca.
#' @import car
#' @export
panorama_ewd = function(sr, ewd, egzamin, typ_szkoly, wskaznik, lata,
                        zapiszPng = NULL, xlab = wskaznik,
                        paleta = colorRampPalette(c("white", blues9)),
                        prElips = NULL, lu = NULL, kolory = NULL) {
  stopifnot(is.numeric(sr), is.numeric(ewd),
            length(sr) == length(ewd),
            is.character(egzamin)   , length(egzamin   ) == 1,
            is.character(typ_szkoly), length(typ_szkoly) == 1,
            is.character(wskaznik)  , length(wskaznik  ) == 1,
            is.character(lata)      , length(lata      ) == 1,
            is.null(zapiszPng) | is.character(zapiszPng),
            is.null(prElips) | is.numeric(prElips),
            is.null(lu) | is.numeric(lu),
            is.null(kolory) | length(kolory) == length(prElips)
            )

  parametryGraficzne = par(no.readonly = TRUE)
  on.exit(par(parametryGraficzne))
  par(bg = "white")
  smoothScatter(sr, ewd, nbin = 256,
                bandwidth = c(ifelse(mean(sr, na.rm = TRUE) > 50, 0.2, 0.02), 0.2),
                main = paste0("Śr. wyniki ", egzamin, " a EWD ",
                              typ_szkoly, "\n",
                              wskaznik, " ", lata),
                xlab = xlab, ylab = paste0("EWD ", wskaznik))

  if (!is.null(prElips) & !is.null(lu)) {
    pWarst = suppressMessages(wielkoscWarstwic(sr, ewd, lu, pr = prElips))
    if (is.null(kolory)) {
      kolory = rep("blue", length(pWarst))
    }
    dataEllipse(sr, ewd, levels = pWarst, plot.points = FALSE, center.pch = NULL,
                fill = TRUE, fill.alpha = 0.1, segments = 200,
                col = kolory, lwd = 1)
  } else if (!is.null(prElips) | !is.null(lu)) {
    message("Jeżeli potrzebujesz użyć elips to oba parametry: prElips i lu muszą być różne od NULL.")
  }

  grid(col = grey(0.5))
  abline(h = 0)
  abline(v = 100)
  temp = data.frame(sr, ewd)
  form = formula("ewd ~ sr")
  m = lm(form, temp)
  abline(m, lwd = 2, lty = 1)
  lines(przew_npar(form, temp), col = 2, lwd = 2)
  legend("bottomright", lwd = 2, lty = 1, col = 1:2,
         legend = c("regr. liniowa", "regr. npar."),
         title = paste0("r = ", format(sqrt(summary(m)$r.squared),
                                       nsmall = 3, digits = 3)),
         bg = "white", cex = 0.8)
  if (is.character(zapiszPng)) {
    katalog = getwd()
    setwd(zapiszPng)
    dev.print(png, paste0("panorama_EWD_", typ_szkoly, "_",
                          ifelse(xlab == wskaznik, "", paste0(xlab, "_")),
                          wskaznik, "_", lata, ".png"),
              height = 1004, width = 1004, pointsize = 12, res = 150)
    setwd(katalog)
  }
  invisible(NULL)
}

#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca "empiryczne prawdopodobieństwa" pozwalające tak dobrać
#' wielkość wastwic, aby znajdował się w ich obrębie założony odsetek uczniów
#' (choć z założenia, że rozkład wyników na wyjściu i EWD jest dwuwymiarowym
#' rozkładem normalnym wynikałaby nieco inna wielkość elips).
#' @param wyniki wektor z wartościami średnich wyników na wyjściu poszczególnych
#' szkół
#' @param ewd wektor z wartościami wskaźników EWD szkół
#' @param liczbaUczniow wektor z liczbą uczniów z poszczególnych szkołach
#' @param pr wektor prawdopodobieństw, dla których mają być wyznaczone
#' odpowiadające im prawdopodobieństwa empiryczne
#' @return wektor liczbowy
#' @import EWDogolny
#' @export
wielkoscWarstwic = function(wyniki, ewd, liczbaUczniow, pr = c(0.5, 0.9)) {
  kow = cov.wt(cbind(wyniki, ewd), liczbaUczniow, method = "ML")
  odlMahSt = mahalanobis(cbind(wyniki, ewd), kow$center, kow$cov)
  kwantyle = kwantyl_wazony(odlMahSt, liczbaUczniow, pr)
  prEmp = round(pchisq(kwantyle, df = 2), 3)
  print(matrix(prEmp, nrow = 1, dimnames = list("pr. emp.", pr)))
  invisible(prEmp)
}
