#' @title Wyliczanie EWD
#' @description
#' Funkcja na podstawie listy modeli, data frame'a z danymi, na podstawie
#' których zostały one wyliczone oraz ew. data frame'a o takiej samej
#' strukturze zawierającego dane nie użyte wcześniej do estymacji modeli,
#' przygotowuje zestawienia z wartościami wskaźników EWD.
#' @param modele lista modeli klasy \code{lm} lub \code{lmerMod}
#' @param dane data frame z dodatkowymi danymi, w szczególności zawierający
#' kolumnę \code{lu_wszyscy}, przypisanymi tym samym obserwacjom, na podstawie
#' których estymowane były modele podane parametrem \code{modele}
#' @param danePominiete opcjonalnie data frame o takiej samej strukturze, jak
#' przekazany argumentem \code{dane}, zawierający dane szkół, które nie zostały
#' wykorzystane na etapie estymacji modelu, ale chcemy teraz wyliczyć dla nich
#' wartości wskaźników
#' @param skale data frame zawierający informacje o skalowaniach (i skalach),
#' z których pochodzą zmienne z oszacowaniami umiejętności, zawarte w danych;
#' typowo atrybut \code{skale} obiektu klasy \code{daneWyskalowane}
#' (zwracanego przez funkcję \code{\link[EWDdane]{przygotuj_dane_do_ewd}})
#' @return lista data frame'ów
#' @import EWDogolny
#' @import plyr
#' @export
przygotuj_wsk_ewd = function(modele, dane, danePominiete = NULL, skale = NULL) {
  stopifnot(is.list(modele), length(modele) > 0,
            is.data.frame(dane),
            is.null(danePominiete) | is.data.frame(danePominiete),
            is.null(skale) | is.data.frame(skale))
  if (!is.null(danePominiete)) {
    stopifnot(all(names(dane) == names(danePominiete)))
  }
  czyLm = all(unlist(lapply(modele, function(x) {return("lm" %in% class(x))})))
  czyLmer = all(unlist(lapply(modele, function(x) {return("lmerMod" %in% class(x))})))
  if (!(czyLm | czyLmer)) {
    stop("Wszystkie elementy listy 'modele' muszą być albo klasy 'lm' albo klasy 'lmerMod'.")
  }
  if (!is.null(skale)) {
    maskaZm = c("id_skali", "opis_skali", "skalowanie", "zmienna")
    stopifnot(all(maskaZm %in% names(skale)))
    skale = skale[, maskaZm]
    skale$opis_skali = sub("^[^;]+;([^;]+);.*$", "\\1", skale$opis_skali)
    names(skale) = sub("^opis_skali", "wskaznik", names(skale))
  }

  zmIdSzk = names(dane)[grep("^id_szkoly_", names(dane))]
  if (length(zmIdSzk) < 1) {
    stop("W danych brak kolumny z id szkoły.")
  }
  names(zmIdSzk) = sub("^id_szkoly_", "", zmIdSzk)
  zmIdSzk = zmIdSzk[order(unlist(list("s" = 1, "g" = 2, "m" = 3)[names(zmIdSzk)]))]
  zmIdSzkWy = zmIdSzk[length(zmIdSzk)]
  zmRokEgzWy = paste0("rok_", names(zmIdSzkWy))

  # wyliczanie EWD i średnich wyników (i ew. korelacji)
  message("Wyliczanie wartości EWD i średnich wyników 'na wyjściu'.")
  if (czyLm) {
    ewd = lapply(modele, ewd_es, idSzkoly = dane[, zmIdSzkWy, drop = FALSE])
    ewdPominiete = lapply(modele, ewd_es,
                          idSzkoly = danePominiete[, zmIdSzkWy, drop = FALSE],
                          noweDane = danePominiete)
  } else {
    ewd = lapply(modele, ewd_me)
    ewdPominiete = lapply(modele, ewd_me_ssr, noweDane = danePominiete)
    ewd = mapply(sr_wy, modele, ewd, SIMPLIFY = FALSE)
    ewdPominiete = mapply(sr_wy, modele, ewdPominiete, SIMPLIFY = FALSE)
  }
  # łączenie wskaźników z danych "normalnych" i "pominiętych"
  ewd = mapply(
    function(x, y) {
      x = rbind(
        cbind(x, pomin = FALSE),
        cbind(y, pomin = TRUE))
      x = subset(x, !is.na(get("ewd")))
      return(x)
    },
    ewd, ewdPominiete, SIMPLIFY = FALSE)
  # wyliczanie srednich wynikow "na wejsciu"
  message("Wyliczanie średnich wyników 'na wejściu'.")
  dane = rbind(dane, danePominiete)
  rm(danePominiete)
  zmEgzWe = lapply(modele, zgadnij_zm_egz_we)
  sr_we = lapply(zmEgzWe, sr_we, dane = dane)
  ewd = mapply(
    function(x, y) {
      return(suppressMessages(join(x, y, type = "left")))
    },
    ewd, sr_we, SIMPLIFY = FALSE)
  rm(sr_we)
  # korekty nazw zmiennych (wzbogacanie o sufiks-nazwę wskaźnika)
  ewd = mapply(
    function(x, rodzajWsk) {
      names(x) = sub("^(|bs_)sr$", paste0("\\1", rodzajWsk), names(x))
      names(x) = sub("^(|bs_)(ewd|kor|sr_we)$", paste0("\\1\\2_", rodzajWsk), names(x))
      return(x)
    },
    ewd, rodzajWsk = as.list(names(ewd)), SIMPLIFY = FALSE
  )
  # dalsze czynności
  message("Dalsze czynności.")
  if (czyLm) {
    maska =  names(dane)[grepl(paste0("_lu$|^lu_wszyscy$"), names(dane))]
    oSzkole = unique(dane[, c(zmIdSzkWy, maska), drop = FALSE])
    ewd = lapply(ewd, function(x, y) {
      return(suppressMessages(join(y, x, type = "right")))
    }, y = oSzkole)
  } else {
    rokDo = max(as.numeric(levels(dane[, zmRokEgzWy])))
    # przygotowywanie obiektu z parametrami modelu
    wskazniki_parametry = ldply(modele, function(x) {
      parametry = summary(x)$coef
      return(data.frame(parametr = rownames(parametry),
                        wartosc = parametry[, 1], bs = parametry[, 2]))
    }, .id = "wskaznik")
    wskazniki_parametry = within(wskazniki_parametry, {
      wskaznik = levels(wskaznik)[wskaznik]
      parametr = levels(parametr)[parametr]
      rodzaj_wsk = "ewd"
      rok_do = rokDo
    })

    lUWszyscy = unique(dane[, c(zmIdSzkWy, "lu_wszyscy")])
    liczba_zdajacych = data.frame()
    wskazniki = data.frame()
    wskazniki_skalowania = data.frame()
    for (i in 1:length(ewd)) {
      message("Wskaźnik ", names(ewd)[i])
      maskaZm = intersect(names(dane), all.vars(formula(modele[[i]])))
      # wypełnianie wskazniki_skalowania
      temp = suppressMessages(
        join(skale, data.frame(zmienna = maskaZm), type = "inner"))
      wskazniki_skalowania =
        rbind(wskazniki_skalowania,
              data.frame(rodzaj_wsk = "ewd", wskaznik = names(modele)[i],
                         rok_do = rokDo, temp[, c("id_skali", "skalowanie")],
                         stringsAsFactors = FALSE))
      # wyliczanie liczby uczniów
      lUWsk = ddply(na.omit(dane[, maskaZm])[, c(zmIdSzkWy, zmRokEgzWy)],
                    unname(zmIdSzkWy),
                    function(x, zmRokEgzWy) {
                      n = nrow(x)
                      return(data.frame(
                        lu = n, lu_ewd = n,
                        roczn_nowy = max(unclass(x[, zmRokEgzWy])),
                        roczn_liczba = length(unique(x[, zmRokEgzWy])),
                        stringsAsFactors = FALSE))
                    },
                    zmRokEgzWy = zmRokEgzWy)
      lUWsk = within(lUWsk, {
        roczn_ostatni = as.numeric(levels(dane[, zmRokEgzWy]))[roczn_nowy]
      })
      lUWsk = within(lUWsk, {
        roczn_nowy = factor(as.numeric(roczn_nowy == max(roczn_nowy)),
                            levels = 0:1, labels = c("nie", "tak"))
      })
      lUWsk = suppressMessages(join(lUWsk, lUWszyscy))
      liczba_zdajacych = rbind(liczba_zdajacych,
                               cbind(id_ww = NA, rodzaj_wsk = "ewd",
                                     wskaznik = names(modele)[i],
                                     kategoria_lu = "ogółem",
                                     lUWsk[, !grepl("^roczn_", names(lUWsk))],
                                     stringsAsFactors = FALSE))
      # przesuwanie średniego wyniku na wyjściu i EWD do średniej ważonej liczbą uczniów w szkole odpowiednio 100 i 0
      ewd[[i]] = suppressMessages(join(ewd[[i]], lUWsk))
      zmEwd   = paste0("ewd_", names(ewd)[i])
      zmWynik = names(ewd)[i]
      przesEwd = with(subset(ewd[[i]], !get("pomin")),
                      weighted.mean(get(zmEwd), lu_ewd))
      przesWyn = with(subset(ewd[[i]], !get("pomin")),
                      weighted.mean(get(zmWynik), lu_ewd))
      ewd[[i]] = within(ewd[[i]], {
        assign(zmEwd, get(zmEwd) - przesEwd)
        assign(zmWynik, get(zmWynik) - przesWyn + 100)
      })
      wskazniki_parametry =
        rbind(wskazniki_parametry,
              data.frame(wskaznik = names(ewd)[i],
                         parametr = c("przesEWD", "przesWynEgzWy"),
                         wartosc = c(przesEwd, przesWyn),
                         bs = NA,  rok_do = rokDo, rodzaj_wsk = "ewd"))
      message("  Przesunięcie średnich wyników końcowych: ", format(przesWyn - 100, nsmall=2, digits=2))
      message("  Przesunięcie EWD: ", format(przesEwd, nsmall=2, digits=2))
      message("  Prawdopodobieństwa empiryczne dla warstwic: ")
      pr = with(subset(ewd[[i]], !get("pomin")),
                wielkoscWarstwic(get(zmWynik), get(zmEwd), lu_ewd))
      wskazniki = rbind(wskazniki,
                        data.frame(rodzaj_wsk = "ewd", wskaznik = names(ewd)[i],
                                   rok_do = rokDo, gamma50 = pr[1], gamma90 = pr[2],
                                   stringsAsFactors = FALSE))
      # przypisywanie kategorii
      ewd[[i]] = within(ewd[[i]], {kategoria = 0})
      ewd[[i]] = within(ewd[[i]], {
        kategoria[get("roczn_liczba") == 2 & get("roczn_nowy") == "tak"] = 1  # wyniki tylko z dwóch roczników, ale są wyniki z najnowszego rocznika
        kategoria[get("lu_ewd") / get("lu_wszyscy") < 0.9              ] = 2  # ponad 10% niepołączonych wyników
        kategoria[get("lu_ewd") < 30                                   ] = 4  # mniej niż 30 połączonych wyników
        kategoria[get("roczn_liczba") < 2                              ] = 5  # dane z tylko jednego rocznika
        kategoria[get("roczn_nowy") == "nie"                           ] = 6  # brak danych z najnowszego rocznika
      })
      message("  Rozkład kategorii:")
      print(with(ewd[[i]], ftable(get("pomin"), get("kategoria"))))
      message("")
      # kosmetyka
      names(ewd[[i]]) = sub("^roczn_ostatni$", "rok", names(ewd[[i]]))
      ewd[[i]] = cbind(ewd[[i]][, !grepl("^roczn_", names(ewd[[i]]))],
                       rok_do = rokDo)
    }
    attributes(ewd)$wskazniki = wskazniki
    attributes(ewd)$wskazniki_skalowania = wskazniki_skalowania
    attributes(ewd)$wskazniki_parametry = wskazniki_parametry
    attributes(ewd)$liczba_zdajacych = liczba_zdajacych
  }
  class(ewd) = c(class(ewd), "listaWskaznikowEWD")
  attributes(ewd)$dataUtworzenia = Sys.time()
  return(ewd)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca oszacowania EWD i ich "błędy standardowe" wykorzystując funkcję
#' \code{\link[EWDogolny]{ranef_ddf}}.
#' @param x model klasy \code{lmerMod}
#' @return data frame
#' @import EWDogolny
#' @export
ewd_me = function(x) {
  if ("lmeEWD" %in% class(x)) {
    stop("Użycie obiektu 'lmeEWD' tylko z funkcją ewd_me_ssr().")
  }

  temp = ranef_ddf(x)
  temp = temp[grepl("^id_(szkoly|gimn|lo|t)", names(temp))][[1]]
  stopifnot(
    ncol(temp) == 3,
    all(names(temp)[2:3] == c("(Intercept)", "csd_(Intercept)"))
  )
  names(temp)[2:3] = c("ewd", "bs_ewd")
  class(temp) = c(class(temp), "wskaznikiEwd")
  return(temp)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca oszacowania EWD i ich "błędy standardowe" wykorzystując metodę
#' "ściągniętych średnich reszt".
#' @details
#' Funkcja powstała jako awaryjne rozwiązanie w sytuacji, gdy
#' \code{ranef(., condVar=TRUE)} nie dawało dobrych oszacowań "błędów
#' standardowych" ze względu na błąd autorów pakietu \code{lme4}.
#' @param x model klasy \code{lmerMod}
#' @param noweDane ramka danych z danymi uczniów dla szkół. Jeżeli NULL to
#' funkcja liczy EWD na danych pobranych z modelu.
#' @return data frame z potencjalnym atrybutem 'noweDane', który zawiera
#' parametr noweDane.
#' @import plyr
#' @export
ewd_me_ssr = function(x, noweDane = NULL) {
  stopifnot(
    length(VarCorr(x)) == 1,
    all(grepl("^id_(szkoly|gimn|lo|t)", names(VarCorr(x)))),
    class(x) %in% c("lmerMod", "lmeEWD")
  )

  if (is.null(noweDane)) {
    resztySt = model.frame(x)[, 1] - predict(x, re.form = ~0)
    grupa = model.frame(x)[, names(VarCorr(x))[1]]
  } else {
    if (class(x) == "lmerMod") {
      resztySt = noweDane[, names(attributes(x)$frame)[1]] -
        predict(x, newdata = noweDane, re.form = ~0)
    } else if (class(x) == "lmeEWD") {
      resztySt = noweDane[, as.character(x$formula[[2]])] -
        predict(x, newdata = noweDane, zLosowymi = FALSE)
    }
    maska = !is.na(resztySt)
    noweDane = noweDane[maska, names(noweDane) %in% all.vars(formula(x))]
    grupa = noweDane[, names(VarCorr(x))[1]]
    resztySt = resztySt[maska]
  }

  sigma2E = sigma(x)^2
  sigma2U = as.numeric(VarCorr(x)[[1]])
  # średnie reszt ściągnięte o czynnik: 1/(1+sigma2E/sigma2U/n)
  # warunkowe odchylenia standardowe to pierwiastek z: wariancja błędów indywidualnych ściągnięta o czynnik j.w., podzielona przez n
  # po kilku przekształceniach: post_sd=1/( n/sigma2E + 1/sigma2U)
  temp = ddply(data.frame(resztySt, grupa, sigma2E, sigma2U), ~grupa, summarise,
               ewd = mean(resztySt) / (1 + sigma2E[1] / sigma2U[1] / length(resztySt)),
               bs_ewd = 1 / sqrt( length(resztySt) / sigma2E[1] + 1 / sigma2U[1] ))
  names(temp)[1] = names(VarCorr(x))[1]
  attributes(temp)$noweDane = noweDane
  class(temp) = c(class(temp), "wskaznikiEwd")

  return(temp)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca oszacowania średnich wyników na wyjściu i ich "błędy
#' standardowe" oraz korelacje z EWD wykorzystując metodę "z jednego modelu".
#' @param model model klasy \code{lmerMod}
#' @param ewd data frame będący wynikiem działania funkcji \code{\link{ewd_me}}
#' lub \code{\link{ewd_me_ssr}}. Jeżeli ten parametr zawiera atrybut noweDane to
#'  funkcja wykonuje wyliczenia na tym atrybucie.
#' @return data frame
#' @import plyr
#' @export
sr_wy = function(model, ewd) {
  stopifnot(
    ncol(ewd) == 3,
    all(grepl("^id_(szkoly|gimn|lo|t)", names(ewd)[1])),
    all(c("ewd", "bs_ewd") %in% names(ewd))
  )
  if (is.factor(ewd[, 1])) ewd[, 1] = as.numeric(levels(ewd[, 1]))[ewd[, 1]]

  noweDane = attributes(ewd)$noweDane
  if (is.null(noweDane)) {
    fit = fitted(model)
    grupa = model.frame(model)[, names(ewd)[1]]
  } else {
    grupa = noweDane[, names(ewd)[1]]
    if (class(model) == "lmerMod") {
      predSt = data.frame(grupa, pred = predict(model, newdata = noweDane, re.form = ~0))
    } else if (class(model) == "lmeEWD") {
      predSt = data.frame(grupa, pred = predict(model, newdata = noweDane,
                                                zLosowymi = FALSE))
    }
    names(predSt)[1] = names(ewd)[1]
    pred_ewd = suppressMessages(join(predSt, ewd ))
    fit = pred_ewd$pred + pred_ewd$ewd
  }

  sr = ddply(data.frame(fit, grupa), ~grupa, summarise,
             sr = mean(fit), bs_sr = sd(fit) / sqrt(length(fit)))
  names(sr)[names(sr) == "grupa"] = names(ewd)[1]
  nrowPrzedPolaczeniemZeSr = nrow(ewd)
  ewd = suppressMessages(join(sr, ewd))
  stopifnot(nrowPrzedPolaczeniemZeSr == nrow(ewd))
  ewd = within(ewd, {bs_sr = (get("bs_sr")^2 + get("bs_ewd")^2)^0.5 })
  ewd = within(ewd, {kor = get("bs_ewd") / bs_sr})
  class(ewd) = unique(c(class(ewd), "wskaznikiEwd"))
  return(ewd)
}
#' @title Wyliczanie EWD (Kalkulator)
#' @description
#' Funkcja zwraca oszacowania EWD i ich błędy standardowe wyliczane jako średnia
#' z reszt regresji MNK oraz średnie wyniki egzaminu na wyjściu i ich błędy
#' standardowe wyliczane jako błąd standardowy średniej z prostej próby losowej.
#' @details
#' Ponieważ obiekt z modelem regresji MNK nie zawiera informacji o przysziale
#' uczniów do szkół, musi ona zostać podana oddzielnym parametrem
#' (\code{id_szkoly}). Musi to być data frame, zawierający kolumnę z id szkoły
#' (i najlepiej tylko nią jedną), utworzony przez usunięcie kolumn z tego samego
#' obiektu, z którego był estymowany model \code{x}. Łączenie jest
#' przeprowadzane po nazwach wierszy (\code{model.frame(x)})' i (\code{id_szkoly}).
#' @param model model klasy \code{lm}
#' @param idSzkoly data frame zawierający kolumnę z identyfikatorami szkół
#' @param noweDane opcjonalnie data frame zawierający nowe dane (tj. nie te,
#' na podstawie których został wystymowany model), dla których mają zostać
#' wyliczone przewidywania, reszty i w efekcie wartości wskaźników
#' @return data frame zawierający oszacowania dla poszczególnych szkół: EWD, błędu
#' standardowego EWD, średniego wyniku końcowegoi jego błędu standardowego
#' @import plyr
#' @export
ewd_es = function(model, idSzkoly, noweDane = NULL) {
  stopifnot(
    "lm" %in% class(model),
    is.data.frame(idSzkoly),
    any(grepl("^id_(szkoly|gimn|lo|t)", names(idSzkoly))),
    is.null(noweDane) | is.data.frame(noweDane)
  )
  if (!is.null(noweDane)) {
    stopifnot((all.vars(formula(model)) %in% names(noweDane)))
  }
  zmIdSzkoly = names(idSzkoly)[grep("^id_(szkoly|gimn|lo|t)($|_)", names(idSzkoly))]
  idSzkoly = data.frame(nrWiersza = rownames(idSzkoly),
                        id_szkoly = idSzkoly[, grep("^id_(szkoly|gimn|lo|t)($|_)",
                                                    names(idSzkoly))],
                        stringsAsFactors = FALSE)
  names(idSzkoly) = sub("id_szkoly", zmIdSzkoly, names(idSzkoly))
  if (is.null(noweDane)) {
    temp = data.frame(nrWiersza = rownames(model.frame(model)),
                      wynik = model.frame(model)[, 1],
                      reszta = model.frame(model)[, 1] - predict(model),
                      stringsAsFactors = FALSE)
  } else {
    temp = data.frame(nrWiersza = rownames(noweDane),
                      wynik = noweDane[, all.vars(formula(model))[1]],
                      reszta = noweDane[, all.vars(formula(model))[1]] -
                        predict(model, newdata = noweDane),
                      stringsAsFactors = FALSE)
  }
  temp = suppressMessages(join(temp, idSzkoly))
  temp = ddply(temp, zmIdSzkoly, summarise,
               ewd = mean(get("reszta")),
               bs_ewd = sd(get("reszta")) / sqrt(length(get("reszta"))),
               sr = mean(get("wynik")),
               bs_sr = sd(get("wynik")) / sqrt(length(get("reszta"))))
  class(temp) = c(class(temp), "wskaznikiEwd")
  return(temp)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja na podstawie modelu stara się zgadnąć nazwę zmiennej opisującej
#' wyniki egzaminu "na wejściu".
#' @details
#' Heurystyka szukania jest następująca: 1) weź zmienne występujące w formule
#' modelu, 2) usuń z nich: pierwszą (tj. zależną) oraz te, które wedle
#' wszlekiego prawdopodobieństwa opisują płeć, dysleksję, rok zdawania egzaminu
#' czy długość toku kształcenia, 3) sposród pozostałych wybierz tą, która
#' najczęściej występuje w nazwach kolumn model.matrix.
#' @param zmEgzWe ciąg znaków - nazwa zmiennej z wynikami egzaminu "na wejściu"
#' @param dane data frame zawierający dane (w szczególności zmieną z id szkoły i
#' zmienną z wynikami egzaminu "na wejściu")
#' @return ciąg znaków (nazwa zmiennej)
#' @import plyr
sr_we = function(zmEgzWe, dane) {
  stopifnot(is.character(zmEgzWe), length(zmEgzWe) == 1,
            is.data.frame(dane))
  stopifnot(zmEgzWe %in% names(dane))
  zmIdSzk = c("s", "g", "m")
  zmIdSzk = paste0("id_szkoly_",
                   zmIdSzk[grep(substr(zmEgzWe, 1, 1), zmIdSzk) + 1])
  stopifnot(zmIdSzk %in% names(dane))

  return(ddply(dane, zmIdSzk, function(x, y) {
    return(data.frame(
      sr_we = mean(x[, y], na.rm = TRUE),
      bs_sr_we = sd(x[, y], na.rm = TRUE) / sqrt(sum(!is.na(x[, y])))
    ))
  }, y = zmEgzWe))
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja na podstawie modelu stara się zgadnąć nazwę zmiennej opisującej
#' wyniki egzaminu "na wejściu".
#' @details
#' Heurystyka szukania jest następująca: 1) weź zmienne występujące w formule
#' modelu, 2) usuń z nich: pierwszą (tj. zależną) oraz te, które wedle
#' wszlekiego prawdopodobieństwa opisują płeć, dysleksję, rok zdawania egzaminu
#' czy długość toku kształcenia, 3) sposród pozostałych wybierz tą, która
#' najczęściej występuje w nazwach kolumn model.matrix.
#' @param model model regresji, typowo klasy \code{lm} lu \code{lmer}
#' @return ciąg znaków (nazwa zmiennej)
zgadnij_zm_egz_we = function(model) {
  zmienne = all.vars(formula(model))[-1]
  zmienne = zmienne[!grepl("^plec$|^wydl|^dysl(eksja)_|^rok_|^laur(|eat)_", zmienne)]
  n = lapply(as.list(zmienne), function(x, y) {return(sum(grepl(x, y)))},
             y = colnames(model.matrix(model)))
  return(zmienne[which.max(n)])
}
