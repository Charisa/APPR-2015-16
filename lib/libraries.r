library(knitr)
library(XML)
library(RCurl)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

# Uvozimo funkcije za delo z datotekami XML.
source("lib/xml.r", encoding = "UTF-8")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")