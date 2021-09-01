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

  info$filename <- file.path(dest, basename(info$url))
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
  process_code(code, region, series)
}


process_code <- function(code, region, series) {
  dat <- jsonlite::fromJSON(find_json(code),
                            simplifyMatrix = FALSE, simplifyDataFrame = FALSE)

  f <- function(x) {
    if (length(x$x) == 0) {
      return(NULL)
    }
    data.frame(x = x$x,
               y = x$y,
               group = x$legendgroup,
               region = region,
               series = series,
               stringsAsFactors = FALSE)
  }

  ret <- dplyr::bind_rows(lapply(dat, f))
  ret$x <- as.Date(ret$x)

  ## Easier to work with as these levels sort numerically
  ret$group <- sub(" anni$", "", ret$group)
  ret$group <- sub("≥90", "90+", ret$group)

  ## Push the "non-verified" data to the end (we might want to work
  ## out where this is)
  ret <- ret[order(ret$group, ret$x), ]
  rownames(ret) <- NULL

  ret
}


process <- function(info) {
  res <- Map(process1, info$region, info$series, info$filename)
  dplyr::bind_rows(res)
}


make_plots <- function(dat) {
  library(ggplot2)
  dir.create("figures", FALSE, TRUE)
  cols <- c("0-9"   = "#3D76CC",
            "10-19" = "#5A3DCC",
            "20-29" = "#AF3DCC",
            "30-39" = "#CC3D93",
            "40-49" = "#CC3D3D",
            "50-59" = "#CC933D",
            "60-69" = "#AFCC3D",
            "70-79" = "#5ACC3D",
            "80-89" = "#3DCC76",
            "90+"   = "#3DCCCC")
  dat$group <- factor(dat$group)
  for (region in unique(dat$region)) {
    for (series in unique(dat$series)) {
      message(sprintf("%s / %s", region, series))
      dest <- sprintf("figures/%s-%s.png", region, series)
      p <- ggplot(dat[dat$region == region & dat$series == series, ],
                  aes(x = x, y = y, group = group)) +
        theme_bw() +
        geom_line(aes(col = group)) +
        scale_colour_manual(values = cols) +
        ggtitle(sprintf("%s %s", region, series)) +
        xlab("date") +
        ylab(series)
      suppressWarnings(
        ggsave(dest, p, "png", width = 1300, height = 360, units = "px",
               dpi = 100))
    }
  }
}
