#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca oszacowania EWD i ich "błędy standardowe" wykorzystując funkcję
#' \code{\link[EWDogolny]{ranef_ddf}}.
#' @param x model klasy \code{lmerMod}
#' @return data frame
#' @import EWDogolny
#' @export
ewd_me = function(x) {
  temp = ranef_ddf(x)
  temp = temp[grepl("^id_(szkoly|gimn|lo|t)", names(temp))][[1]]
  stopifnot(
    ncol(temp) == 3,
    all(names(temp)[2:3] == c("(Intercept)", "csd_(Intercept)"))
  )
  names(temp)[2:3] = c("ewd", "bs_ewd")
  return(temp)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca oszacowania EWD i ich "błędy standardowe" wykorzystując metodę
#' "ściągniętych średnich reszt".
#' @details
#' Funkcja powstała jako awaryjne rozwiązanie w sytuacji, gdy
#' \code{ranef(., condVar=TRUE)} nie dawało dobrych oszacowań "błędów standardowych"
#' ze względu na błąd autorów pakietu \code{lme4}.
#' @param x model klasy \code{lmerMod}
#' @param noweDane ramka danych z danymi uczniów dla szkół. Jeżeli NULL to funkcja liczy EWD na danych pobranych z modelu.
#' @return data frame z potencjalnym atrybutem 'noweDane', który zawiera parametr noweDane.
#' @import plyr
#' @export
ewd_me_ssr = function(x, noweDane = NULL) {
  stopifnot(
    length(VarCorr(x))==1,
    all(grepl("^id_(szkoly|gimn|lo|t)", names(VarCorr(x))))
  )

  if (is.null(noweDane)) {
    resztySt = model.frame(x)[, 1] - predict(x, re.form=~0)
    grupa = model.frame(x)[, names(VarCorr(x))[1]]
  } else {
    resztySt = noweDane[, names(attributes(x)$frame)[1]] - predict(x, newdata = noweDane, re.form = ~0)
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

  return(temp)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca oszacowania średnich wyników na wyjściu i ich "błędy standardowe"
#' oraz korelacje z EWD wykorzystując metodę "z jednego modelu".
#' @param model model klasy \code{lmerMod}
#' @param ewd data frame będący wynikiem działania funkcji \code{\link{ewd_me}} lub \code{\link{ewd_me_ssr}}.
#' Jeżeli ten parametr zawiera atrybut noweDane to funkcja wykonuje wyliczenia na tym atrybucie.
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
    predSt = data.frame(grupa, pred = predict(model, newdata = noweDane, re.form = ~0))
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
  ewd = within(ewd, {
    kor = bs_ewd / bs_sr
    bs_sr = (bs_sr^2 + bs_ewd^2)^0.5 })
  return(ewd)
}
#' @title Wyliczanie EWD (Kalkulator)
#' @description
#' Funkcja zwraca oszacowania EWD i ich błędy standardowe wyliczane jako średnia z reszt
#' regresji MNK oraz średnie wyniki egzaminu na wyjściu i ich błędy standardowe wyliczane
#' jako błąd standardowy średniej z prostej próby losowej.
#' @details
#' Ponieważ obiekt z modelem regresji MNK nie zawiera informacji o przysziale uczniów do
#' szkół, musi ona zostać podana oddzielnym parametrem (\code{id_szkoly}). Musi to być
#' data frame, zawierający kolumnę z id szkoły (i najlepiej tylko nią jedną), utworzony
#' przez usunięcie kolumn z tego samego obiektu, z którego był estymowany model \code{x}.
#' Łączenie jest przeprowadzane po nazwach wierszy (\code{model.frame(x)})
#' i (\code{id_szkoly}).
#' @param x model klasy \code{lm}
#' @param idSzkoly data frame zawierający kolumnę z identyfikatorami szkół
#' @return data frame zawierający oszacowania dla poszczególnych szkół: EWD, błędu
#' standardowego EWD, średniego wyniku końcowegoi jego błędu standardowego
#' @import plyr
#' @export
ewd_es = function(x, idSzkoly) {
  stopifnot(
    "lm" %in% class(x),
    is.data.frame(idSzkoly),
    any(grepl("^id_(szkoly|gimn|lo|t)", names(idSzkoly)))
  )
  zmIdSzkoly = names(idSzkoly)[grep("^id_(szkoly|gimn|lo|t)($|_)", names(idSzkoly))]
  idSzkoly = data.frame(nrWiersza = rownames(idSzkoly),
                         id_szkoly = idSzkoly[, grep("^id_(szkoly|gimn|lo|t)($|_)", names(idSzkoly))],
                         stringsAsFactors=FALSE)
  names(idSzkoly) = sub("id_szkoly", zmIdSzkoly, names(idSzkoly))
  temp = data.frame(nrWiersza = rownames(model.frame(x)),
                    wynik = model.frame(x)[, 1],
                    reszta = model.frame(x)[, 1] - predict(x),
                    stringsAsFactors=FALSE)
  temp = suppressMessages(join(temp, idSzkoly))
  temp = ddply(temp, zmIdSzkoly, summarise,
               ewd = mean(get("reszta")),
               bs_ewd = sd(get("reszta")) / sqrt(length(get("reszta"))),
               sr = mean(get("wynik")),
               bs_sr = sd(get("wynik")) / sqrt(length(get("reszta"))))
  return(temp)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca "empiryczne prawdopodobieństwa" pozwalające tak dobrać wielkość wastwic,
#' aby znajdował się w ich obrębie założony odsetek uczniów (choć z założenia, że rozkład
#' wyników na wyjściu i EWD jest dwuwymiarowym rozkładem normalnym wynikałaby nieco inna
#' wielkość elips).
#' @param wyniki wektor z wartościami średnich wyników na wyjściu poszczególnych szkół
#' @param ewd wektor z wartościami wskaźników EWD szkół
#' @param liczbaUczniow wektor z liczbą uczniów z poszczególnych szkołach
#' @param pr wektor prawdopodobieństw, dla których mają być wyznaczone odpowiadające im
#' prawdopodobieństwa empiryczne
#' @return wektor liczbowy
#' @import EWDogolny
#' @export
wielkoscWarstwic = function(wyniki, ewd, liczbaUczniow, pr=c(0.5, 0.9)) {
  kow = cov.wt(cbind(wyniki, ewd), liczbaUczniow, method="ML")
  odlMahSt = mahalanobis(cbind(wyniki, ewd), kow$center, kow$cov)
  kwantyle = kwantyl_wazony(odlMahSt, liczbaUczniow, pr)
  prEmp = round(pchisq(kwantyle, df=2), 3)
  print(matrix(prEmp, nrow=1, dimnames=list("pr. emp.", pr)))
  invisible(prEmp)
}
