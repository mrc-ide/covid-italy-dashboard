data_info <- function() {
  regions <- c("abruzzo",
               "basilicata",
               "campania",
               "calabria",
               "emilia_romagna",
               "friuli_venezia_giulia",
               "lazio",
               "liguria",
               "lombardia",
               "marche",
               "molise",
               "piemonte",
               "puglia",
               "sardegna",
               "sicilia",
               "toscana",
               "trentino_alto_adige",
               "umbria",
               "valle_daosta",
               "veneto",
               "italia",
               "pa_bolzano",
               "pa_trento")

  series <- c("positivi",
              "ricoveri",
              "deceduti",
              "sintomatici",
              "terapia_intensiva")

  ret <- expand.grid(series = series,
                     region = regions[1:20],
                     stringsAsFactors = FALSE)
  url_base <- "https://covid19.infn.it/iss/plots"
  ret$url <- sprintf("%s/iss_age_date_%s_%s.div",
                     url_base, ret$region, ret$series)
  ret
}


download <- function(info, dest) {
  dir.create(dest, FALSE, TRUE)
  for (u in info$url) {
    path <- file.path(dest, basename(u))
    if (!file.exists(path)) {
      message(path)
      dat <- readLines(u, warn = FALSE)
      writeLines(dat, path)
      Sys.sleep(runif(1, max = 3))
    }
  }

  info$filename <- file.path(dest, basename(u))
  info
}


find_json <- function(code, parse = FALSE) {
  if (parse) {
    dat <- js::esprima_tokenize(code, loc = TRUE)
    depth <- cumsum(
    (tmp$type == "Punctuator" & tmp$value == "[") -
    (tmp$type == "Punctuator" & tmp$value == "]"))
    i <- which(depth == 1)[[1]]
    j <- which(depth == 0 & seq_along(depth) > i)[[1]]
    json <- substr(code,
                   tmp[i, ]$loc$end$column,
                   tmp[j, ]$loc$end$column)
  } else {
    chars <- strsplit(code, NULL)[[1]]
    depth <- cumsum((chars == "[") - (chars == "]"))
    i <- which(depth == 1)[[1]]
    j <- which(depth == 0 & seq_along(depth) > i)[[1]]
    json <- substr(code, i, j)
  }
  json
}


process1 <- function(region, series, filename) {
  message(sprintf("%s / %s", region, series))
  txt <- readLines(filename)
  dat <- xml2::read_html(txt)
  code <- xml2::xml_text(xml2::xml_find_first(dat, "//script"))
  dat <- jsonlite::fromJSON(find_json(code),
                            simplifyMatrix = FALSE, simplifyDataFrame = FALSE)

  f <- function(x) {
    data.frame(x = as.Date(x$x),
               y = x$y,
               group = x$legendgroup,
               region = region,
               series = series,
               stringsAsFactors = FALSE)
  }

  dplyr::bind_rows(lapply(dat, f))
}


process <- function(info) {
  res <- Map(process1, info$region, info$series, info$filename)
  dplyr::bind_rows(res)
}
