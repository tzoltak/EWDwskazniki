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
#' @return data frame
#' @export
ewd_me_ssr = function(x) {
  stopifnot(
    length(VarCorr(x))==1,
    all(grepl("^id_(szkoly|gimn|lo|t)", names(VarCorr(x))))
  )

  resztySt = model.frame(x)[, 1] - predict(x, re.form=~0)
  sigma2E = sigma(x)^2
  sigma2U = VarCorr(x)[[1]]
  # średnie reszt ściągnięte o czynnik: 1/(1+sigma2E/sigma2U/n)
  # warunkowe odchylenia standardowe to pierwiastek z: wariancja błędów indywidualnych ściągnięta o czynnik j.w., podzielona przez n
  # po kilku przekształceniach: post_sd=1/( n/sigma2E + 1/sigma2U)
  temp = merge(
    aggregate(data.frame(efLos=resztySt),
              list(grupa=model.frame(x)[, names(VarCorr(x))[1]]),
              function(x, stosWar) {
                return( mean(x) / (1 + stosWar / length(x)) )
              },
              stosWar = sigma2E / sigma2U),
    aggregate(data.frame(csd_efLos=resztySt),
              list(grupa=model.frame(x)[, names(VarCorr(x))[1]]),
              function(x, sigma2E, sigma2U) {
                return( 1 / sqrt( length(x) / sigma2E + 1 / sigma2U ) )
              },
              sigma2E = sigma2E, sigma2U = sigma2U),
    by="grupa"
  )
  names(temp) = c(names(VarCorr(x))[1], "ewd", "bs_ewd")
  return(temp)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca oszacowania średnich wyników na wyjściu i ich "błędy standardowe"
#' oraz korelacje z EWD wykorzystując metodę "z jednego modelu".
#' @param model model klasy \code{lmerMod}
#' @param ewd data frame będący wynikiem działania funkcji \code{\link{ewd_me}} lub \code{\link{ewd_me_ssr}}
#' @return data frame
#' @export
sr_wy = function(model, ewd) {
  stopifnot(
    ncol(ewd) == 3,
    all(grepl("^id_(szkoly|gimn|lo|t)", names(ewd)[1])),
    all(c("ewd", "bs_ewd") %in% names(ewd))
  )
  sr = merge(
    aggregate(data.frame(sr   =fitted(model)),
              list(grupa=model.frame(model)[, names(ewd)[1]]),
              mean),
    aggregate(data.frame(bs_sr=fitted(model)),
              list(grupa=model.frame(model)[, names(ewd)[1]]),
              function(x) {return( sd(x) / sqrt(length(x)) )})
  )
  names(sr)[names(sr) == "grupa"] = names(ewd)[1]
  nrowPrzedPolaczeniemZeSr = nrow(ewd)
  ewd = merge(sr, ewd)
  stopifnot(nrowPrzedPolaczeniemZeSr == nrow(ewd))
  ewd = within(ewd, {bs_sr = (bs_sr^2 + bs_ewd^2)^0.5 })
  ewd = within(ewd, {kor = bs_ewd / bs_sr})
  return(ewd)
}
#' @title Wyliczanie EWD
#' @description
#' Funkcja zwraca "empiryczne prawdopodobieństwa" pozwalające tak dobrać wielkość wastwic,
#' aby znajdował się w ich obrębie założony odseteke uczniów (choć z założenia, że
#' rozkład wyników na wyjściu i EWD jest dwuwymiarowym rozkładem normalnym wynikałaby
#' nieco inna wielkość elips).
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
