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
    amount %<>% str_replace_all("l", "1", .)
    errorFlag <- as.numeric(amount) %>% is.na()
    if (errorFlag) {
      amount <- readline(prompt=paste(amount, "appears to be non-numeric. What should the value be? "))
    }
    cur <- str_extract(textBlock[AIDX], "(?<=CUR:\\s{0,3})[\\S]+")
    #common OCR mistake is confusing "USD" with "USO", so replace that
    cur <- ifelse(cur == "USO", "USD", cur)
    MIDX <- grep("ORIG TO BNF INFO:", textBlock, ignore.case=T)[1] + 1
    memo <- textBlock[MIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
      gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    #BoA Wires have a different format depending on whether a customer received a wire or sent one.
    #If a BoA customer *sends* a wire, it will say "**** CREDIT PAYMENT MESSAGE TEXT ****" on the wire.
    if (any(grepl("CREDIT PAYMENT MESSAGE TEXT", textBlock))) {
      s <- grep("CREDIT PAYMENT MESSAGE TEXT", textBlock)
      if (any(grepl("SWIFT Message Text", textBlock))) f <- grep("SWIFT Message Text", textBlock) else f <- length(textBlock)
      messageBlock <- textBlock[s:f]

      OBIDX <- grep("Sending Bank:", messageBlock, ignore.case=T)[1] + 3
      oBank <- messageBlock[OBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      BBIDX <- grep("Receiving Bank:", messageBlock, ignore.case=T)[1] + 3
      bBank <- messageBlock[BBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      if (any(grepl("Beneficiary's Bank", messageBlock, ignore.case=T))) {
        iBank <- bBank
        BBIDX <- grep("Beneficiary's Bank", messageBlock, ignore.case=T)[1] + 1
        bBank <- messageBlock[BBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
          gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      } else {
        iBank <- NA
      }
      BIDX <- grep("Beneficiary:", messageBlock, ignore.case=T)[1] + 1
      bnf <- messageBlock[BIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      OIDX <- grep("Originator:", messageBlock, ignore.case=T) + 1
      orig <- messageBlock[OIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    } else {
      bBank <- "Bank of America"
      OBIDX <- grep("DEBIT VAL:\\s?../../..", textBlock, ignore.case=T)[1] + 1
      bnf <- textBlock[OBIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      OIDX <- grep("ORIG:\\s*/\\s*[A-Z0-9]+", textBlock, ignore.case=T)[1] + 1
      orig <- textBlock[OIDX] %>% gsub("^\\s+|\\s+$", "", .) %>% str_split(., "\\s{2,}") %>% unlist() %>% .[1] %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      oBank <- textBlock[OBIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[1] %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      if (any(grepl("ORDERING BNK:", textBlock, ignore.case=T))) {
        iBank <- oBank
        OBIDX <- grep("ORDERING BNK:", textBlock, ignore.case=T)[1] + 1
        oBank <- textBlock[OBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
          gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
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
  txt %<>% str_replace_all("^\\s+|\\s+$", "")

  SIDX <- grep("MIF_AMOUNT", txt)
  FIDX <- c(tail(SIDX, -1) - 1, length(txt))
  dat_m <- do.call(rbind, lapply(1:length(SIDX), function(idx) {
    s <- SIDX[idx]
    f <- FIDX[idx]
    textBlock <- txt[s:f]

    DIDX <- grep("^Wire Date", textBlock, ignore.case=T)
    date <- str_extract(textBlock[DIDX], "\\d+/\\d+/\\d+")
    AIDX <- grep("^Amount", textBlock, ignore.case=T)
    amount <- str_extract(textBlock[AIDX], "[0-9,\\.l]+") %>% str_replace_all("[\\s,]", "")
    #common OCR mistake is to confuse a "1" with an "l", so replace it
    amount %<>% str_replace_all("l", "1")
    errorFlag <- as.numeric(amount) %>% is.na()
    if (errorFlag) {
      amount <- readline(prompt=paste(amount, "appears to be non-numeric. What should the value be? "))
    }
    CIDX <- grep("^Currency", textBlock, ignore.case=T)
    cur <- str_extract(textBlock[CIDX], "[A-Z]{3}$")
    #common OCR mistake is confusing "USD" with "USO", so replace that
    cur <- ifelse(cur == "USO", "USD", cur)
    TIDX <- grep("^Time", textBlock, ignore.case=T)
    time <- str_extract(textBlock[TIDX], "..:..:..$")
    MIDX <- grep("^OBI\\s+", textBlock)[1]
    memo <- str_extract(textBlock[MIDX], "(?<=OBI\\s{1,10})[A-z]+(\\s?([A-z]+)?){0,}") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    BIDX <- grep("^Beneficiary", textBlock, ignore.case=T)[1]
    bnf <- str_extract(textBlock[BIDX], "(?<=Beneficiary\\s{1,10})[A-z]+(\\s?([A-z]+)?){0,}") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    BBIDX <- grep("^Recv Name", textBlock, ignore.case=T)[1]
    bBank <- str_extract(textBlock[BBIDX], "(?<=Recv Name\\s{1,10})[A-z]+(\\s?([A-z]+)?){0,}") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    BANIDX <- grep("^BNF ID", textBlock)[1]
    bAcctNum <- str_extract(textBlock[BANIDX], "(?<=BNF ID\\s{1,10})[A-z0-9]+")
    IBIDX <- grep("^Intermd Bank", textBlock, ignore.case=T)[1]
    iBank <- str_extract(textBlock[IBIDX], "(?<=Intermd Bank\\s{1,10})[A-z]+(\\s?([A-z]+)?){0,}") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    OIDX <- grep("^Originator", textBlock, ignore.case=T)[1]
    orig <- str_extract(textBlock[OIDX], "(?<=Originator\\s{1,10})[A-z]+(\\s?([A-z]+)?){0,}") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    OBIDX <- grep("^Sender Name", textBlock, ignore.case=T)[1]
    oBank <- str_extract(textBlock[OBIDX], "(?<=Sender Name\\s{1,10})[A-z]+(\\s?([A-z]+)?){0,}") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    OANIDX <- grep("^ORG ID", textBlock, ignore.case=T)[1]
    oAcctNum <- str_extract(textBlock[OANIDX], "(?<=ORG ID\\s{1,10})[A-z0-9]+")

    return(c("Date"=date, "Time"=time, "Amount"=amount, "Currency"=cur, "Originator"=orig,
             "originatorAcctNum"=oAcctNum, "originatorBank"=oBank,
             "intermediaryBank"=iBank, "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
             "Beneficiary"=bnf, "Memo"=memo))
  }))
  dat <- dat_m %>% as.data.frame(stringsAsFactors=F)
  return(dat)
}

parseCitibank <- function(file, n) {
  type = file_ext(file)
  if (type %in% c("xls", "xlsx", "csv")) {
    if (type == "csv") tmp <- read_csv(file, skip=n) else tmp <- read_excel(file, skip=n)
    if (ncol(tmp) != 12) tmp <- tmp[, 1:12]
    defaultNames <- c("Global Id", "Instr Dt", "Originator", "Orig. Bank", "DB or INTER. Party",
                      "Debited Party", "Credited Party", "CR or INTER. Party", "Bene. Bank",
                      "Beneficiary", "Amount", "Orig Bene Info ")
    if (!all(names(tmp) %in% defaultNames)) stop("This tool requires a specific set of variables for Citibank spreadsheets. Please see the documentation for this tool.")
    names(tmp) <- c("globalID", "instructionDate", "originator", "originatorBank", "dbOrInterParty",
                    "debitedParty", "creditedParty", "crOrInterParty", "beneficiaryBank", "beneficiary",
                    "amount", "OBI")
    tmp <- apply(tmp, 2, function(z) ifelse(z %in% c("()", " "), NA, z)) %>%
      as.data.frame(stringsAsFactors=F)
    tmp %<>% filter(rowSums(is.na(.)) != ncol(.))

    date <- tmp$instructionDate
    amount <- tmp$amount
    cur <- "USD"
    time <- "00:00"
    memo <- tmp$OBI
    orig <- tmp$originator %>% str_replace("\\s+\\(.*\\)", "") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    oBank <- ifelse(is.na(tmp$originatorBank), "Citibank", tmp$originatorBank) %>%
      str_replace("\\s+\\(.*\\)", "") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    oAcctNum <- str_extract(tmp$originator, "(?<=\\s{1,10})\\(.*\\)") %>%
      str_replace_all("\\(|\\)", "")
    bnf <- tmp$beneficiary %>% str_replace("\\s+\\(.*\\)", "") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    bBank <- ifelse(is.na(tmp$beneficiaryBank), "Citibank", tmp$beneficiaryBank) %>%
      str_replace("\\s+\\(.*\\)", "") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    bAcctNum <- str_extract(tmp$beneficiary, "(?<=\\s{1,10})\\(.*\\)") %>%
      str_replace_all("\\(|\\)", "")
    #If both intermediary party fields are empty and Citibank isn't mentioned, then they
    #are the intermediary bank. Otherwise, Citibank should be in either the originatingBank
    #field or the beneficiaryBank field. The intermediate bank then will be whatever is in
    #either of the intermediary party fields. If both intermediary party fields are populated
    #then we need to figure out what to do.
    iBank <- "placeholder"
    dat <- data.frame("Date"=date, "Time"=time, "Amount"=amount, "Currency"=cur,
                      "Originator"=orig, "originatorAcctNum"=oAcctNum,
                      "originatorBank"=oBank, "intermediaryBank"=iBank,
                      "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
                      "Beneficiary"=bnf, "Memo"=memo, stringsAsFactors=F)
  } else {
    txt <- read_lines(file)
    txt %<>% str_replace_all("\\\t|\\s{2,}", " ") %>% str_replace("^\\s+|\\s+$", "")
  }
}

parseHSBC <- function(file) {
  type <- file_ext(file)
  sheetName <- "Details"
  if (type == "xls") {
    if (!("Details" %in% .Call('readxl_xls_sheets', PACKAGE='readxl', file))) {
      sheetName <- readline(prompt="In which sheet is the data located? ")
    }
  } else {
    if (!("Details" %in% .Call('readxl_xlsx_sheets', PACKAGE='readxl', file))) {
      sheetName <- readline(prompt="In which sheet is the data located? ")
    }
  }
  tmp <- read_excel(file, sheet=sheetName)
  names(tmp) %<>% gsub("_", " ", .) %>%
    gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    gsub("\\s", "", .)
  date <- tmp$TransactionDate
  amount <- tmp$Amount
  cur <- tmp$CcyCodeCurrency
  time <- "00:00"
  memo <- "Placeholder"

}


parseWireData <- function(file, bank, format, skip=0) {
  if (!file.exists(file)) stop("Invalid file path. Please be sure to use the full file path to a valid file.")
  if (tolower(bank) == "bank of america") {
    if (tolower(format) != "pdf") stop("Bank of America typically sends PDF files. Are you sure you sure this is the right format?")
    return(parseBOA(file))
  }
  if (tolower(bank) == "bnymellon") {
    if (tolower(format) != "xlsx") stop("BNY Mellon typically sends XLSX files. Are you sure this is the right format?")
    return(parseBNY(file))
  }
  if (tolower(bank) == "capital one") {
    if (tolower(format) != "pdf") stop("Capital One typically sends PDF files. Are you sure this is the right format?")
    return(parseCapOne(file))
  }
  if (tolower(bank) == "citibank") {
    if (!(tolower(format) %in% c("xls", "xlsx", "csv"))) stop("Citibank typically sends both a Word document and an Excel document. The Excel document is preferred for this tool, so please use that file.")
    return(parseCitibank(file, n=skip, type=format))
  }
}


