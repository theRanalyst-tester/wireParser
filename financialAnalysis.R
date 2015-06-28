library(readr)
library(tools)
library(dplyr)
library(magrittr)
library(stringr)
library(XML)
library(zoo)
library(maps)
library(geosphere)
library(sna)
library(network)
library(RColorBrewer)

parseBOA <- function(file) {
  dat_m <- NULL
  cmd <- paste('pdftotext -layout "', file, '"', sep='')
  system(cmd)
  txtFile <- file %>% file_path_sans_ext() %>% paste(., '.txt', sep='')
  txt <- read_lines(txtFile)

  SIDX <- grep("<\\s<\\s<\\s", txt)
  FIDX <- c(tail(SIDX, -1) - 1, length(txt))
  dat_m <- do.call(rbind, lapply(1:length(SIDX), function(idx) {
    s <- SIDX[idx]
    f <- FIDX[idx]
    textBlock <- txt[s:f]

    #in the event that more than one index is returned, I want to keep only the first
    DIDX <- grep("SND DATE:", textBlock)[1]
    date <- str_extract(textBlock[DIDX], "(?<=SND DATE:\\s?)../../..")
    AIDX <- grep("RPT#\\s*AMT:", textBlock)[1]
    amount <- str_extract(textBlock[AIDX], "(?<=AMT:\\s{0,3})[\\s0-9.,l]+") %>%
      str_replace_all("[\\s,]", "")
    #common OCR mistake is to confuse a "1" with an "l", so replace it
    amount %<>% gsub("l", "1", .)
    errorFlag <- as.numeric(amount) %>% is.na()
    if (errorFlag) {
      amount <- readline(prompt=paste(amount, "appears to be non-numeric. What should the value be? "))
    }
    cur <- str_extract(textBlock[AIDX], "(?<=CUR:\\s{0,3})[\\S]+")
    #common OCR mistake is confusing "USD" with "USO", so replace that
    cur[cur == "USO"] <- "USD"
    MIDX <- grep("ORIG TO BNF INFO:", textBlock, ignore.case=T)[1] + 1
    memo <- textBlock[MIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
      gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)

    #BoA Wires have a different format depending on whether a customer received a wire or sent one.
    #If a BoA customer *sends* a wire, it will say "**** CREDIT PAYMENT MESSAGE TEXT ****" on the wire.
    if (any(grepl("CREDIT PAYMENT MESSAGE TEXT", textBlock))) {
      s <- grep("CREDIT PAYMENT MESSAGE TEXT", textBlock)
      if (any(grepl("SWIFT Message Text", textBlock))) f <- grep("SWIFT Message Text", textBlock) else f <- length(textBlock)
      messageBlock <- textBlock[s:f]

      OBIDX <- grep("Sending Bank:", messageBlock, ignore.case=T)[1] + 3
      oBank <- messageBlock[OBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
        gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      BBIDX <- grep("Receiving Bank:", messageBlock, ignore.case=T)[1] + 3
      bBank <- messageBlock[BBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
        gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      if (any(grepl("Beneficiary's Bank", messageBlock, ignore.case=T))) {
        iBank <- bBank
        BBIDX <- grep("Beneficiary's Bank", messageBlock, ignore.case=T)[1] + 1
        bBank <- messageBlock[BBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>% gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      } else {
        iBank <- NA
      }
      BIDX <- grep("Beneficiary:", messageBlock, ignore.case=T)[1] + 1
      bnf <- messageBlock[BIDX] %>% gsub("^\\s+|\\s+$", "", .) %>% gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      OIDX <- grep("Originator:", messageBlock, ignore.case=T) + 1
      orig <- messageBlock[OIDX] %>% gsub("^\\s+|\\s+$", "", .) %>% gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
    } else {
      bBank <- "Bank of America"
      OBIDX <- grep("DEBIT VAL:\\s?../../..", textBlock, ignore.case=T)[1] + 1
      bnf <- textBlock[OBIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
        gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      OIDX <- grep("ORIG:\\s*/\\s*[A-Z0-9]+", textBlock, ignore.case=T)[1] + 1
      orig <- textBlock[OIDX] %>% gsub("^\\s+|\\s+$", "", .) %>% str_split(., "\\s{2,}") %>% unlist() %>% .[1] %>%
        gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      oBank <- textBlock[OBIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[1] %>%
        gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      if (any(grepl("ORDERING BNK:", textBlock, ignore.case=T))) {
        iBank <- oBank
        OBIDX <- grep("ORDERING BNK:", textBlock, ignore.case=T)[1] + 1
        oBank <- textBlock[OBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
          gsub("([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      } else {
        iBank <- NA
      }
      oAcctNum <- str_extract(textBlock[OIDX - 1], "(?<=ORIG\\s?:\\s?/\\s?)[A-Z0-9]+")
      BANIDX <- grep("BNF:", textBlock)
      bAcctNum <- str_extract(textBlock[BANIDX], "(?<=BNF\\s?:\\s?/\\s?)[A-Z0-9]+")
    }

    return(c("Date"=date, "Amount"=amount, "Currency"=cur, "Originator"=orig,
             "originatorAcctNum"=oAcctNum, "originatorBank"=oBank,
             "intermediaryBank"=iBank, "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
             "Beneficiary"=bnf, "Memo"=memo))
  }))
  dat <- dat_m %>% as.data.frame(stringsAsFactors=F)
  return(dat)
}

parseBNYMellon <- function(file) {
  dat_m <- NULL
  temp <- read_excel(file, col_names=F)
  txt <- temp$X0
  SIDX <- grep("PAYMT\\s+TRN\\s+[A-Z0-9]{16}", txt)
  FIDX <- c(tail(SIDX, -1) - 1, length(txt))
  dat_m <- do.call(rbind, lapply(1:length(SIDX), function(idx) {
    s <- SIDX[idx]
    f <- FIDX[idx]
    textBlock <- txt[s:f]
  }))
}

parseCapOne <- function(file) {
  dat_m <- NULL
  cmd <- paste('pdftotext -layout "', file, '"', sep='')
  system(cmd)
  txtFile <- file %>% file_path_sans_ext() %>% paste(., '.txt', sep='')
  txt <- read_lines(txtFile)

  SIDX <- grep("MIF_AMOUNT", txt)
  FIDX <- c(tail(SIDX, -1) - 1, length(txt))
  dat_m <- do.call(rbind, lapply(1:length(SIDX), function(idx) {
    s <- SIDX[idx]
    f <- FIDX[idx]
    textBlock <- txt[s:f]


  }))

parseWireData <- function(file, bank, format) {
  if (!file.exists(file)) stop("Invalid file path. Please be sure to use the full file path to a valid file.")
  if (tolower(bank) == "bank of america") {
    if (tolower(format) != "pdf") stop("Bank of America typically sends PDF files. Are you sure you sure this is the right format?")
    parseBOA(file)
  }
  if (tolower(bank) == "bnymellon") {
    if (tolower(format) != "xlsx") stop("BNY Mellon typically sends XLSX files. Are you sure this is the right format?")
    parseBNY(file)
  }
  if (tolower(bank) == "capital one") {
    if (tolower(format) != "pdf") stop("Capital One typically sends PDF files. Are you sure this is the right format?")
    parseCapOne(file)
  }
}


