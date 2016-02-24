# Uvoz potrebnih knjižnic

source("lib/libraries.r", encoding = "UTF-8")


# Funkciji za uvoz zemljevidov v 4. fazi




pretvori.zemljevid <- function(zemljevid) {
  fo <- fortify(zemljevid)
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}





uvozi.zemljevid <- function(url, pot.zemljevida, mapa = "zemljevidi",
                            encoding = "UTF-8", force = FALSE){
  ime.zemljevida <- digest(url, algo = "sha1")
  map <- paste0(mapa, "/", ime.zemljevida)
  pot <- paste0(map, "/", pot.zemljevida)
  shp <- paste0(pot, ".shp")
  zip <- paste0(map, "/", ime.zemljevida, ".zip")
  if (force || !file.exists(shp)) {
    if (!file.exists(map)) {
      dir.create(map, recursive = TRUE)
    }
    download.file(url, zip)
    unzip(zip, exdir = map)
  }
  re <- paste0("^", gsub("\\.", "\\.", pot.zemljevida), "\\.")
  files <- grep(paste0(re, "[a-z0-9.]*$"),
                grep(paste0(re, ".*$"), dir(map, recursive = TRUE), value = TRUE),
                value = TRUE, invert = TRUE)
  file.rename(paste0(map, "/", files),
              paste0(map, "/", sapply(strsplit(files, "\\."),
                                      function(x)
                                        paste(c(x[1], tolower(x[2:length(x)])),
                                              collapse = "."))))
  zemljevid <- readShapeSpatial(shp)
  
  for (col in names(zemljevid)) {
    if (is.factor(zemljevid[[col]])) {
      zemljevid[[col]] <- factor(iconv(zemljevid[[col]], encoding))
    }
  }
  
  return(zemljevid)
}
