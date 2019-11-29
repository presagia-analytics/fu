
#' Get the Right Hand Terms of a Formula
#'
#' @param formula the formula to extract the right hand terms from.
#' @param data an associated data set. Default NULL (no dataset)
#' @export
rh_terms <- function(form, data = NULL) {
#  ret <- gsub("`", "", attributes(terms(form))$term.labels)
  ret <- attributes(terms(form, data = data))$term.labels
  if (length(ret) == 1 && length(grep("\\|", ret)) > 0) {
    rh_split <- strsplit(ret, "\\|")[[1]]
    ret <- lapply(rh_split,
      function(x) {
        nsp <- gsub(" ", "", x)
        if (nsp == ".") {
          nsp
        } else {
          gsub("`", "", 
            attributes(terms(as.formula(paste("~", x))))$term.labels)
        }
      })
    names(ret) <- c("indep", "cond")
  } else {
    ret <- list(indep = ret, cond = NULL)
  }
  if (attributes(terms(form, data = data))$intercept == 0) {
    ret$indep <- c(ret$indep, "-1")
  }
  ret
}

#' Get the Independent Terms of a Formula
#'
#' @param formula the formula to extract the right hand terms from.
#' @param data an associated data set. Default NULL (no dataset)
#' @export
indep_terms <- function(form, data = NULL) {
  rh_terms <- rh_terms(form, data)$indep
}

#' Get the Conditional Terms of a Formula
#'
#' @param formula the formula to extract the right hand terms from.
#' @param data an associated data set. Default NULL (no dataset)
#' @export
cond_terms <- function(form, data = NULL) {
  rh_terms <- rh_terms(form)$cond
}

#' Get the Left Hand Terms
#'
#' @param formula the formula to extract the right hand terms from.
#' @param data an associated data set. Default NULL (no dataset)
#' @export
dep_terms <- function(form, data = NULL) {
  ret <- NULL
  facs <- attributes(terms(form, data = data))$factors
  lht_all <- rownames(attributes(terms(form, data = data))$factors)[1]
  all.vars(parse(text = lht_all))
}

un_interact <- function(x) {
  unique(unlist(strsplit(x, ":")))
}

#' Get a Description of a formula Object
#'
#' @param x a data.frame.
#' @param form a formula object.
#' @param lhs_must_appear should we stop if an lhs variable doesn't appear in 
#' the data? Default FALSE.
#' @importFrom crayon red
#' @export
form_desc <- function(x, form, lhs_must_appear = FALSE) {
  ft <- c(list(dep = dep_terms(form, x)), rh_terms(form, x))
  if (isTRUE(any(duplicated(unlist(ft))))) {
    stop(red("Variables may not be apear more than once"))
  }
  dt_list_elem <- which(unlist(lapply(ft, function(x) "." %in% x)))
  if (length(dt_list_elem) > 0) {
    dt <- which("."  == ft[[dt_list_elem]])
    ft[[dt_list_elem]] <- c(ft[[dt_list_elem]][-dt], 
      setdiff(colnames(x), setdiff(unlist(ft), ".")))
  }
  if (lhs_must_appear) {
    check_vars <- unlist(ft)
  } else {
    check_vars <- unlist(c(ft$indep, ft$cond))
  }
  if (isTRUE(!all(un_interact(check_vars) %in% colnames(x)))) {
    stop(red("The following formula variables do not appear in data set.\n\t",
             paste(setdiff(check_vars, colnames(x)), collapse = "\n\t"),
             sep = ""))
  }
  ft
}

#' Expand a Formula with a dot
#'
#' @param x the data set.
#' @param form the formula.
#' @importFrom crayon red 
#' @export
expand_formula <- function(x, form) {
  ft <- form_desc(x, form)
  ret <- paste(ft$dep, "~", paste(ft$indep, collapse = " + "))
  if (!is.null(ft$cond)) {
    ret <- paste(ret, " | ", paste(ft$cond, collapse = " + "))
  }
  as.formula(ret)
}

#' Make a formula
#'
#' @param dep_vars the dependent variables.
#' @param indep_vars the independent variables.
#' @param cond_vars the conditional variables. (Default NULL - none)
#' @export
make_formula <- function(dep_vars, indep_vars, cond_vars = NULL) {
  ret <- paste(dep_vars, "~", paste(indep_vars, collapse = " + "))
  if (!is.null(cond_vars)) {
    ret <- paste(ret, "|", cond_vars)
  }  
  as.formula(ret)
}


