#' @title Konwersja obiektu lme4 na lmeEWD
#' @description
#' Funkcja tworzy obiekt klasy \code{lmeEWD} na podstawie obiektu \code{lmerMod}.
#' @param objectLME4 model klasy \code{lmerMod}
#' @return obiekt klasy \code{lmeEWD}
#' @importFrom stats as.formula formula getCall sigma
#' @export
konwertujNalmeEWD <- function(objectLME4){
  stopifnot(  class(objectLME4) == "lmerMod")
  ret = list()
  ret[["formula"]] = as.formula(formula(getCall(objectLME4)))
  ret[["sigma"]] = sigma(objectLME4)
  ret[["VarCorr"]] = VarCorr(objectLME4)
  ret[["fixef"]] = fixef(objectLME4)
  class(ret) = "lmeEWD"
  ret
}
sigma.lmeEWD <- function(x){
  x$sigma
}
VarCorr.lmeEWD <- function(x){
  x$VarCorr
}
fixef.lmeEWD <- function(x){
  x$fixef
}
#' @title Przewidywanie dla obiektu 'lmeEWD'
#' @description
#' Funkcja tworzy obiekt klasy 'lmeEWD' na podstawie obiektu 'lmerMod'.
#' @param object model klasy \code{lmeEWD}
#' @param ... parametry: \code{newdata} - ramka danych, dla których należy
#' obliczyć przewidywania oraz \code{zLosowymi} -  wartość logiczna określająca,
#' czy uwzględniać w przewidywaniach efekt losowy
#' @return wektor liczb
#' @importFrom stats formula model.matrix
#' @export
predict.lmeEWD <- function(object, ...){
  dots <- list(...)
  newdata = dots$newdata
  zLosowymi = dots$zLosowymi

  form = object$formula
  form <- nobars(form[[length(form)]])
  if (is.null(form)) {
    form = 1
  }

  RHS <- formula(substitute(~R, list(R = form)))
  X = model.matrix(RHS, newdata)
  pred = drop(X %*% object$fixef)

  if (zLosowymi) {
    # zaszyta rekurencja w funkcji ewd_me_ssr(), która korzysta
    # z predict.lmeEWD() bez efektow losowych
    ewd = ewd_me_ssr(object, noweDane = newdata)[, -3]
    retJoin = join(newdata, ewd)
    pred = pred + retJoin$ewd
  }
  pred
}
