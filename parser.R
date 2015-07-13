library(readr)
library(readxl)
library(tools)
library(dplyr)
library(magrittr)
library(stringr)
library(stringdist)

setwd("~/Documents/wireParser/")
#Ensure that large amounts of money are not recorded in scientific notation or rounded
options(scipen=999)
options(digits=15)

clean_entities <- function(data) {
  #Add some cleanup logic for entities and banks
  data$Originator %<>% str_replace_all("(?<=Llc\\b|Inc\\b|Ltd\\b|Co\\b).*", "") %>%
    str_replace_all("\\(.*\\)", "") %>%
    str_replace_all("[\\.,]", "") %>%
    str_trim() %>%
    gsub("\\b([A-z]{2,3})$", "\\U\\1", ., perl=T) %>%
    gsub("^([A-z]{2,3})\\b", "\\U\\1", ., perl=T) %>%
    str_replace_all("\\bCO$", "Corporation") %>%
    str_replace_all("\\bLTD$|\\bLT$", "Limited") %>%
    str_replace_all("\\bINC$", "Incorporated") %>%
    str_replace_all("\\bPty\\b", "Proprietary") %>%
    str_replace_all("^\\d/", "") %>%
    str_replace_all("(?<=\\w)\\d/(?=\\w)", ", ") %>%
    str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "")
  data$Beneficiary %<>% str_replace_all("(?<=Llc\\b|Inc\\b|Ltd\\b|Co\\b).*", "") %>%
    str_replace_all("\\(.*\\)", "") %>%
    str_replace_all("[\\.,]", "") %>%
    str_trim() %>%
    gsub("\\b([A-z]{2,3})$", "\\U\\1", ., perl=T) %>%
    gsub("^([A-z]{2,3})\\b", "\\U\\1", ., perl=T) %>%
    str_replace_all("\\bCO$", "Corporation") %>%
    str_replace_all("\\bLTD$|\\bLT$", "Limited") %>%
    str_replace_all("\\bINC$", "Incorporated") %>%
    str_replace_all("\\bPty\\b", "Proprietary") %>%
    str_replace_all("^\\d/", "") %>%
    str_replace_all("(?<=\\w)\\d/(?=\\w)", ", ") %>%
    str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "")
  data$originatorBank %<>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    str_replace_all("Jpmorgan", "JPMorgan") %>%
    str_replace_all("Hsbc", "HSBC") %>%
    gsub("\\b([A-z]{2,3})$", "\\U\\1", ., perl=T) %>%
    gsub("^([A-z]{2,3})\\b", "\\U\\1", ., perl=T) %>%
    str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
    str_replace_all("\\s\\d+.*", "") %>%
    str_replace_all("bank of new york mellon, nyc", "BNY Mellon") %>%
    str_replace_all("\\s?-\\s?.*", "") %>%
    str_replace_all("PTY|Pty|pty", "Proprietary") %>%
    str_replace_all("LTD|Ltd|ltd", "Limited") %>%
    str_replace_all("\\.,", "")
  data$intermediaryBank %<>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    str_replace_all("Jpmorgan", "JPMorgan") %>%
    str_replace_all("Hsbc", "HSBC") %>%
    gsub("\\b([A-z]{2,3})$", "\\U\\1", ., perl=T) %>%
    gsub("^([A-z]{2,3})\\b", "\\U\\1", ., perl=T) %>%
    str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
    str_replace_all("\\s\\d+.*", "") %>%
    str_replace_all("bank of new york mellon, nyc", "BNY Mellon") %>%
    str_replace_all("\\s?-\\s?.*", "") %>%
    str_replace_all("PTY|Pty|pty", "Proprietary") %>%
    str_replace_all("LTD|Ltd|ltd", "Limited") %>%
    str_replace_all("\\.", "")
  data$beneficiaryBank %<>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    str_replace_all("Jpmorgan", "JPMorgan") %>%
    str_replace_all("Hsbc", "HSBC") %>%
    gsub("\\b([A-z]{2,3})$", "\\U\\1", ., perl=T) %>%
    gsub("^([A-z]{2,3})\\b", "\\U\\1", ., perl=T) %>%
    str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
    str_replace_all("\\s\\d+.*", "") %>%
    str_replace_all("bank of new york mellon, nyc", "BNY Mellon") %>%
    str_replace_all("\\s?-\\s?.*", "") %>%
    str_replace_all("PTY|Pty|pty", "Proprietary") %>%
    str_replace_all("LTD|Ltd|ltd", "Limited") %>%
    str_replace_all("\\.,", "")

  return(data)
}

load_excel <- function(file, sheet=1, skip=0, colNames=T) {
  format <- file_ext(file)
  if (format == "csv") {
    n <- read_csv(file, n_max=1) %>% names() %>% length()
    colTypes <- rep("c", n) %>% paste(., collapse='')
    tmp <- read_csv(file, col_types=colTypes, col_names=colNames, skip=skipNumber)
  } else {
    #check for sheet name in sheet names
    functionName <- paste("readxl", format, "sheets", sep="_")
    sheetNames <- .Call(functionName, PACKAGE="readxl", file)
    while (!sheet %in% sheetNames) {
      sheet <- readline(prompt="What is the name of the data sheet? ")
    }
    #get number of columns and create a vector of column types
    sheetIndex <- which(sheetNames == sheet)
    if (colNames) {
      functionName <- paste("readxl", format, "col_names", sep="_")
      colNames <- .Call(functionName, PACKAGE = 'readxl', file, sheetIndex, skip)
      n <- length(colNames)
      colTypes <- rep("text", n)
      tmp <- read_excel(file, col_types=colTypes, sheet=sheet, skip=skip)
    } else {
      #This is for files like those from Bank of New York Mellon
      tmp <- read_excel(file, sheet, col_names=F)
    }
  }
  names(tmp) %<>% str_replace_all("_", " ") %>%
    str_trim() %>%
    gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    str_replace_all("\\s?/\\s?", " ") %>%
    str_replace_all(" ", "")
  #want variables names to be camel case if they're more than one word, otherwise capitalized
  names(tmp) %<>% gsub("^([A-Z])(?=[a-z]+[A-Z])", "\\L\\1", ., perl=T)

  return(tmp)
}

parse_boa <- function(file) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (tolower(format) != "pdf") stop("This function expects a PDF file. ",
                                     "Please use an appropriate file.")
  cmd <- paste('pdftotext -layout "', file, '"', sep='')
  system(cmd)
  txtFile <- file %>% file_path_sans_ext() %>% paste(., '.txt', sep='')
  txt <- read_lines(txtFile)

  SIDX <- grep("<\\s<\\s<\\s", txt)
  FIDX <- c(tail(SIDX, -1) - 1, length(txt))
  datM <- do.call(rbind, lapply(1:length(SIDX), function(idx) {
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
    amount %<>% str_replace_all("l", "1")
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
      bAddr <- textBlock[(BIDX+1):(BIDX+2)] %>% paste(., collapse=" ")
      OIDX <- grep("Originator:", messageBlock, ignore.case=T) + 1
      orig <- messageBlock[OIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      oAddr <- textBlock[(OIDX+1):(OIDX+2)] %>% paste(., collapse=" ")
    } else {
      bBank <- "Bank of America"
      OBIDX <- grep("DEBIT VAL:\\s?../../..", textBlock, ignore.case=T)[1] + 1
      bnf <- textBlock[OBIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2] %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      bAddr <- textBlock[(OBIDX+1):(OBIDX+2)] %>% str_split(., "\\s{3,}") %>%
        unlist() %>% .[c(2, 4)] %>% paste(., collapse=" ")
      OIDX <- grep("ORIG:\\s*/\\s*[A-Z0-9]+", textBlock, ignore.case=T)[1] + 1
      orig <- textBlock[OIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        str_split(., "\\s{2,}") %>% unlist() %>% .[1] %>%
        gsub("([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      oAddr <- textBlock[(OIDX+1):(OIDX+2)] %>%
        str_split(., "\\s{3,}") %>% unlist()
      if (length(oAddr) > 2) {
        oAddr %<>% .[c(1, 3)] %>% paste(., collapse=" ")
      } else {
        oAddr %<>% paste(., collapse=" ")
      }
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
             "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediaryBank"=iBank,
             "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
             "beneficiaryAddress"=bAddr, "Beneficiary"=bnf, "Memo"=memo))
  }))
  dat <- datM %>% as.data.frame(stringsAsFactors=F)
  dat %<>% clean_entities()
  return(dat)
}

parse_bny <- function(file) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (!tolower(format) %in% c("csv", "xls", "xlsx")) stop("This function expects ",
                                                          "an Excel file. ",
                                                          "Please use an ",
                                                          "appropriate file.")
  tmp <- load_excel(file, colNames=F)
  txt <- temp$X0
  SIDX <- grep("PAYMT\\s+TRN\\s+[A-Z0-9]{16}", txt)
  FIDX <- c(tail(SIDX, -1) - 1, length(txt))
  datM <- do.call(rbind, lapply(1:length(SIDX), function(idx) {
    s <- SIDX[idx]
    f <- FIDX[idx]
    textBlock <- txt[s:f]
  }))
  dat %<>% clean_entities()
  return(dat)
}

parse_capone <- function(file) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (tolower(format) != "pdf") stop("This function expects a PDF file. ",
                                     "Please use an appropriate file.")
  cmd <- paste('pdftotext -layout "', file, '"', sep='')
  system(cmd)
  txtFile <- file %>% file_path_sans_ext() %>% paste(., '.txt', sep='')
  txt <- read_lines(txtFile)
  txt %<>% str_replace_all("^\\s+|\\s+$", "")

  SIDX <- grep("MIF_AMOUNT", txt)
  FIDX <- c(tail(SIDX, -1) - 1, length(txt))
  datM <- do.call(rbind, lapply(1:length(SIDX), function(idx) {
    s <- SIDX[idx]
    f <- FIDX[idx]
    textBlock <- txt[s:f]

    #common data
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
    cur <- str_extract(textBlock[CIDX], "(?<=Currency\\s{1,10}).*")
    #common OCR mistake is confusing "USD" with "USO", so replace that
    cur[cur == "USO"] <- "USD"
    MIDX <- grep("^OBI\\s+", textBlock)[1]
    memo <- str_extract(textBlock[MIDX], "(?<=OBI\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    #originator/beneficiary info
    OIDX <- grep("^Originator", textBlock, ignore.case=T)[1]
    orig <- str_extract(textBlock[OIDX], "(?<=Originator\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    OAIDX <- grep("ORG ADDR", textBlock)[1]
    oAddr <- paste(textBlock[OAIDX:(OAIDX+2)], collapse=" ") %>%
      str_replace_all("ORG ADDR\\d", "")
    #Because BNF ID matches the account number, I assume ORG ID is the
    #Originator Account Number
    OANIDX <- grep("^ORG ID", textBlock, ignore.case=T)[1]
    oAcctNum <- str_extract(textBlock[OANIDX], "(?<=ORG ID\\s{1,10}).*")

    BIDX <- grep("^Beneficiary", textBlock, ignore.case=T)[1]
    bnf <- str_extract(textBlock[BIDX], "(?<=Beneficiary\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    BAIDX <- grep("^BNF ADDR", textBlock)[1]
    bAddr <- paste(textBlock[BAIDX:(BAIDX+2)], collapse=" ") %>%
      str_replace_all("BNF ADDR\\d", "")
    BANIDX <- grep("^BNF ID", textBlock)[1]
    bAcctNum <- str_extract(textBlock[BANIDX], "(?<=BNF ID\\s{1,10}).*")

    #bank info
    #There is no "Orig Bank" field, but Sender Name appears to be the
    #field we're looking for.
    OBIDX <- grep("^Sender Name", textBlock, ignore.case=T)[1]
    oBank <- str_extract(textBlock[OBIDX], "(?<=Sender Name\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    BBIDX <- grep("^Bene Bank", textBlock, ignore.case=T)[1]
    bBank <- str_extract(textBlock[BBIDX], "(?<=Bene Bank\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    IBIDX <- grep("^Intermd Bank", textBlock, ignore.case=T)[1]
    iBank <- str_extract(textBlock[IBIDX], "(?<=Intermd Bank\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    #There is a fourth bank field called Recv Name. This field appears to never
    #be blank, but Bene Bank and Intermd Bank can be, so need to set this field
    #and then run some logic.
    RBIDX <- grep("^Recv Name", textBlock, ignore.case=T)[1]
    rBank <- str_extract(textBlock[RBIDX], "(?<=Recv Name\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    #There are two possibilities: 1) Recv Bank is the Intermediary Bank or
    # 2) it's the Beneficiary Bank. Because Recv Bank appears to never be empty,
    #I'm assuming that it is the actual beneficiary bank.
    if (is.na(bBank)) {
      bBank <- rBank
    } else {
      if (is.na(iBank)) {
        iBank <- bBank
        bBank <- rBank
      } else {
        iBank <- paste(iBank, bBank, sep=", ")
        bBank <- rBank
      }
    }

    return(c("Date"=date, "Amount"=amount, "Currency"=cur, "Originator"=orig,
             "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediaryBank"=iBank,
             "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
             "beneficiaryAddress"=bAddr, "Beneficiary"=bnf, "Memo"=memo))
  }))
  dat <- dat_m %>% as.data.frame(stringsAsFactors=F)
  dat %<>% clean_entities()
  return(dat)
}

parse_citibank <- function(file, skip=1) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (!tolower(format) %in% c("csv", "xls", "xlsx", "doc", "docx")) {
    stop("This function expects an Excel or Word file. Please use an appropriate file.")
  }
  if (format %in% c("xls", "xlsx", "csv")) {
    #load the file and clean it up
    tmp <- load_excel(file, skipNumber=skip)
    if (ncol(tmp) != 12) tmp <- tmp[, 1:12]
    #For now, I'm going to make a bold assumption that the variable names will stay the
    #same. In the future, I'll need to figure out a way to dynamically identify the
    #appropriate names. But that's probably true for all of these files...
    names(tmp) <- c("globalID", "instructionDate", "originator", "originatorBank",
                    "dbOrInterParty", "debitedParty", "creditedParty", "crOrInterParty",
                    "beneficiaryBank", "beneficiary","amount", "OBI")
    tmp <- apply(tmp, 2, function(z) ifelse(z %in% c("()", " "), NA, z)) %>%
      as.data.frame(stringsAsFactors=F)
    tmp %<>% filter(rowSums(is.na(.)) != ncol(.))

    #common data
    date <- tmp$instructionDate
    amount <- tmp$amount
    cur <- "USD"
    memo <- tmp$OBI %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)

    #originator/beneficiary info
    orig <- tmp$originator %>% str_replace("\\s+\\(.*\\)", "") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    oAddr <- NA
    oAcctNum <- str_extract(tmp$originator, "(?<=\\s{1,10})\\(.*\\)") %>%
      str_replace_all("\\(|\\)", "")
    bnf <- tmp$beneficiary %>% str_replace("\\s+\\(.*\\)", "") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    bAddr <- NA
    bAcctNum <- str_extract(tmp$beneficiary, "(?<=\\s{1,10})\\(.*\\)") %>%
      str_replace_all("\\(|\\)", "")

    #bank info
    oBank <- apply(tmp, 1, function(row) {
      if (is.na(row['originatorBank'])) {
        oBank <- "Citibank"
      } else {
        oBankValues <- row['originatorBank'] %>% str_split("\\(") %>%
          .[[1]] %>% str_replace_all("[\\s{2,}\\)]", "")
        swiftFlag <- oBankValues[1] == oBankValues[2]
        if (swiftFlag) oBank <- row['dbOrInterParty'] else oBank <- row['originatorBank']
      }
      oBank
    })
    oBank %<>% str_replace_all("\\(.*\\)|\\s{2,}", "") %>%
      gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
      str_replace_all("^\\s+|\\s+$", "")

    bBank <- apply(tmp, 1, function(row) {
      if (is.na(row['beneficiaryBank'])) {
        bBank <- "Citibank"
      } else {
        bBankValues <- row['beneficiaryBank'] %>% str_split("\\(") %>%
          .[[1]] %>% str_replace_all("[\\s{2,}\\)]", "")
        swiftFlag <- bBankValues[1] == bBankValues[2]
        if (swiftFlag) bBank <- row['crOrInterParty'] else bBank <- row['beneficiaryBank']
      }
      bBank
    })
    bBank %<>% str_replace_all("\\(.*\\)|\\s{2,}", "") %>%
      gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
      str_replace_all("^\\s+|\\s+$", "")

    #Account for when DB/CR or Inter Party is different from the Orig/Benef Bank
    #and the Orig/Benef bank is not a SWIFT code
    dbi <- apply(tmp, 1, function(row) {
      if (!is.na(row['dbOrInterParty'])) {
        dbiValue <- row['dbOrInterParty'] %>% str_replace_all("\\(.*\\)", "") %>%
          str_replace_all("^\\s+|\\s+$", "") %>% tolower()
        if (is.na(dbiValue)) dbiValue <- ""
        origBankValue <- row['originatorBank'] %>% str_replace_all("\\(.*\\)", "") %>%
          str_replace_all("^\\s+|\\s+$", "") %>% tolower()
        if (is.na(origBankValue)) origBankValue <- ""
        if (stringdist(dbiValue, origBankValue)/nchar(dbiValue) > 1/3) {
          oBankValues <- row['originatorBank'] %>% str_split("\\(") %>%
            .[[1]] %>% str_replace_all("[\\s{2,}\\)]", "")
          swiftFlag <- oBankValues[1] == oBankValues[2]
          if (!swiftFlag) iBank <- dbiValue else iBank <- NA
        } else {
          iBank <- NA
        }
      } else {
        iBank <- NA
      }
      iBank
    })
    dbi %<>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
    cri <- apply(tmp, 1, function(row) {
      if (!is.na(row['crOrInterParty'])) {
        criValue <- row['crOrInterParty'] %>% str_replace_all("\\(.*\\)", "") %>%
          str_replace_all("^\\s+|\\s+$", "")
        beneBankValue <- row['beneficiaryBank'] %>% str_replace_all("\\(.*\\)", "") %>%
          str_replace_all("^\\s+|\\s+$", "")
        if (stringdist(criValue, beneBankValue)/nchar(criValue) > 1/3) {
          bBankValues <- row['beneficiaryBank'] %>% str_split("\\(") %>%
            .[[1]] %>% str_replace_all("[\\s{2,}\\)]", "")
          swiftFlag <- bBankValues[1] == bBankValues[2]
          if (!swiftFlag) iBank <- criValue else iBank <- NA
        } else {
          iBank <- NA
        }
      } else {
        iBank <- NA
      }
      iBank
    })
    cri %<>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
    dbiFlag <- !is.na(dbi)
    criFlag <- !is.na(cri)
    iBank <- ifelse(dbiFlag & criFlag, "Need to reconcile", ifelse(dbiFlag, dbi, cri))

    #Check if Citibank is any of the 2-3 banks we've established, and if not
    #then set it as an intermediate bank
    bBankFlag <- grepl("Citi\\s?bank", bBank, ignore.case=T)
    oBankFlag <- grepl("Citi\\s?bank", oBank, ignore.case=T)
    iBankFlag <- grepl("Citi\\s?bank", iBank, ignore.case=T)
    iBankNA <- is.na(iBank)
    iBank <- sapply(1:length(iBankFlag), function(idx) {
      if (!any(bBankFlag[idx], oBankFlag[idx], iBankFlag[idx])) {
        if (iBankNA[idx]) val <- "Citibank" else val <- paste(iBank[idx], "Citibank", sep=", ")
      } else {
        val <- iBank[idx]
      }
      val
    })
    #For now, I'm going to ignore the Debited Party and Credited Party fields
    #as I think they're useless
    dat <- data.frame("Date"=date, "Amount"=amount, "Currency"=cur,
                      "Originator"=orig, "originatorAddress"=oAddr,
                      "originatorAcctNum"=oAcctNum,
                      "originatorBank"=oBank, "intermediaryBank"=iBank,
                      "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
                      "beneficiaryAddress"=bAddr, "Beneficiary"=bnf,
                      "Memo"=memo, stringsAsFactors=F)
  } else {
    txt <- read_lines(file)
    txt %<>% str_replace_all("\\\t|\\s{2,}", " ") %>% str_replace("^\\s+|\\s+$", "")
  }

  dat %<>% clean_entities()
  return(dat)
}

parse_hsbc <- function(file) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (!tolower(format) %in% c("csv", "xls", "xlsx")) stop("This function expects ",
                                                          "an Excel file. ",
                                                          "Please use an ",
                                                          "appropriate file.")

  tmp <- load_excel(file, sheet="Details")
  tmp %<>% filter(rowSums(is.na(.)) != ncol(.))

  #common data
  date <- tmp$transactionDate %>% as.Date(format="%m/%d/%Y")
  amount <- tmp$Amount
  amount <- ifelse(grepl("\\.", amount), amount, paste(amount, "00", sep="."))
  cur <- tmp$ccyCodeCurrency
  memo <- apply(tmp, 1, function(row) {
    if (is.na(row['senderBankCorresp'])) {
      if (is.na(row['receiverBankCorresp'])) {
        val <- NA
      } else {
        val <- paste("Recieving Bank:", row['receiverBankCorresp'])
      }
    } else {
      if (is.na(row['recieverBankCorresp'])) {
        val <- paste("Sending Bank:", row['senderBankCorresp'])
      } else {
        val <- paste("Sending Bank:", row['senderBankCorresp'],
                     "Recieving Bank:", row['receiverBankCorresp'])
      }
    }
    val
  })

  #If the Beneficiary listed is a bank, HSBC appends data in *Seqb fields.
  #It's possible that the Originator is also listed as a bank, which, to the
  #best of my knowledge, only occurs if the Beneficiary is a bank. This can be
  #determined by comparing the Originator to the OriginatorSeqb field. Either
  #way, I need to assign variables based on whether or not the Beneficiary is
  #a bank. The returned value will be a matrix that will need to be transposed.
  vars <- apply(tmp, 1, function(row) {
    #If there's no *Seqb variables, assign as usual
    if (is.na(row['originatorSeqb'])) {
      #originator/beneficiary info
      origField <- row['Originator'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
      orig <- origField[2] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      oAcctNum <- origField[1]
      if (length(origField) >= 3) {
        oAddr <- origField %>% .[3:length(.)] %>%
          str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
          .[nchar(.)> 3] %>% paste(., collapse=" ")
      } else {
        oAddr <- NA
      }
      bnfField <- row['Beneficiary'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
      bnf <- bnfField[2] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      bAcctNum <- bnfField[1]
      if (length(bnfField) >= 3) {
        bAddr <- bnfField %>% .[3:length(.)] %>%
          str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
          .[nchar(.)> 3] %>% paste(., collapse=" ")
      } else {
        bAddr <- NA
      }

      #bank info
      #Originator Bank can be empty, so if that's the case set it to be the debit party.
      if (is.na(row['originatorBank'])) {
        oBank <- row['DebitParty'] %>%
          gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
          str_replace_all("^\\s+|\\s+$", "")
      } else {
        #Sometimes only the SWIFT code is included in a Bank field so the bank comes
        #back as NA. In this case, set it as the SWIFT code and will replace later.
        oBank <- row['originatorBank'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2] %>%
          gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
          str_replace_all("^\\s+|\\s+$", "")
        if (is.na(oBank)) oBank <- row['originatorBank'] %>% str_split("\\s{3}") %>%
          .[[1]] %>% .[1] %>% str_replace_all("^\\s+|\\s+$", "")
      }
      #BeneficiaryBank is always empty, so set it to Credit Party.
      #Credit Party has been blank in some instances. In this case, set it to "Unknown".
      bBank <- row['creditParty'] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all("^\\s+|\\s+$", "")
      if (is.na(bBank)) bBank <- "Unknown"

      #There are two possibilities for the intermediary bank if Originator Bank is
      #not empty: 1) Debit Party 2) None iBank will be NA if Originator Bank is empty.
      if (is.na(row['originatorBank'])) {
        iBank <- NA
      } else {
        dpValue <- row['debitParty']
        obValue <- row['originatorBank']
        if (grepl(dpValue, obValue, fixed=T)) {
          iBank <- NA
        } else {
          iBank <- row['debitParty'] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
            str_replace_all("^\\s+|\\s$", "")
        }
      }

      #There have been edge cases where the Debit Party and Beneficiary are the same value
      #and as a result HSBC is listed no where in the record. Need to rememdy this.
      if (bnf == bBank) {
        hsbcFlag <- grepl("hsbc", c(oBank, bBank, iBank), ignore.case=T) %>% sum() > 0
        if (hsbcFlag) {
          bBank <- "HSBC"
        } else {
          bBank <- "Unknown"
        }
      }
      v <- c("Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediateBank"=iBank, "beneficiaryBank"=bBank,
             "beneficiaryAcctNum"=bAcctNum, "beneficiaryAddress"=bAddr, "Beneficiary"=bnf)

    #Otherwise there are *Seqb variables, which means the Originator and/or the
    #Beneficiary fields are banks and need to be reset to the actual entities.
    } else {
      #Check if Originator == OriginatorSeqb. If not, then the Originator is (presumably)
      #a bank and should be reset to the actual entity.
      if (row['Originator'] == row['originatorSeqb']) {
        origField <- row['Originator'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
        orig <- origField[2] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
        oAcctNum <- origField[1]
        if (length(origField) >= 3) {
          oAddr <- origField %>% .[3:length(.)] %>%
            str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
            .[nchar(.)> 3] %>% paste(., collapse=" ")
        } else {
          oAddr <- NA
        }
        #There are some instances where Originator Bank are NA. In this event, use the
        #Debit party as the Originator Bank
        if (is.na(row['originatorBank'])) {
          oBank <- row['debitParty'] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
        } else {
          oBank <- row['originatorBank'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2] %>%
            gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
            str_replace_all("^\\s+|\\s+$", "")
          if (is.na(oBank)) oBank <- row['originatorBank'] %>% str_split("\\s{3}") %>%
            .[[1]] %>% .[1] %>% str_replace_all("^\\s+|\\s+$", "")
        }
        #There are four possibilities for the Intermediary Bank if Originator Bank
        #is not empty: 1) Credit Party 2) Debit Party 3) Both or 4) Neither
        #There are only two possibilities for the Intermediary Bank if it is:
        # 1) Credit Party or 2) None
        if (is.na(row['originatorBank'])) {
          cpValue <- row['creditParty']
          bbValue <- row['Beneficiary']
          if (grepl(cpValue, bbValue, fixed=T)) {
            iBank <- NA
          } else {
            iBank <- cpValue %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
              str_replace_all("^\\s+|\\s+$", "")
          }
        } else {
          dpValue <- row['debitParty']
          cpValue <- row['creditParty']
          obValue <- row['originatorBank']
          bbValue <- row['Beneficiary']
          if (grepl(dpValue, obValue, fixed=T) & grepl(cpValue, bbValue, fixed=T)) {
            iBank <- NA
          } else if (grepl(dpValue, obValue, fixed=T) & !grepl(cpValue, bbValue, fixed=T)) {
            iBank <- cpValue %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
              str_replace_all("^\\s+|\\s+$", "")
          } else if (!grepl(dpValue, obValue, fixed=T) & grepl(cpValue, bbValue, fixed=T)) {
            iBank <- dpValue %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
              str_replace_all("^\\s+|\\s+$", "")
          } else {
            iBank1 <- dpValue %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
              str_replace_all("^\\s+|\\s+$", "")
            iBank2 <- cpValue %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
              str_replace_all("^\\s+|\\s+$", "")
            iBank <- paste(iBank1, iBank2, sep=", ")
          }
        }
      } else {
        origField <- row['originatorSeqb'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
        orig <- origField[2] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
        oAcctNum <- origField[1]
        if (length(origField) >= 3) {
          oAddr <- origField %>% .[3:length(.)] %>%
            str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
            .[nchar(.)> 3] %>% paste(., collapse=" ")
        } else {
          oAddr <- NA
        }
        #There are some instances in which the OriginatorSeqb field is a SWIFT code
        #that doesn't match any of the banks listed, so that needs to be the Originator
        #bank and the DebitParty and Originator need to be listed as intermediary banks.
        dpCheck <- row['debitParty']
        origCheck <- row['Originator']
        if (!is.na(row['originatorBankSeqb']) & grepl(dpCheck, origCheck, ignore.case=T)) {
          oBank <- row['originatorBankSeqb']
        } else {
          oBank <- row['Originator'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2] %>%
            gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
            str_replace_all("^\\s+|\\s+$", "")
          if (is.na(oBank)) oBank <- row['originatorBank'] %>% str_split("\\s{3}") %>%
              .[[1]] %>% .[1] %>% str_replace_all("^\\s+|\\s+$", "")
        }
        iBank <- row['debitParty'] %>%
          gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
          str_replace_all("^\\s+|\\s+$", "")
      }
      #Beneficiary Bank should be in the Beneficiary field and the Beneficiary should
      #be in the BeneficiarySeqb field.
      bnfField <- row['beneficiarySeqb'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
      bnf <- bnfField[2] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      bAcctNum <- bnfField[1]
      if (length(bnfField) >= 3) {
        bAddr <- bnfField %>% .[3:length(.)] %>%
          str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
          .[nchar(.)> 3] %>% paste(., collapse=" ")
      } else {
        bAddr <- NA
      }
      bBank <- row['Beneficiary'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2] %>%
        gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all("^\\s+|\\s+$", "")
      if (is.na(bBank)) bBank <- row['Beneficiary'] %>% str_split("\\s{3}") %>%
        .[[1]] %>% .[1] %>% str_replace_all("^\\s+|\\s+$", "")

      v <- c("Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediateBank"=iBank, "beneficiaryBank"=bBank,
             "beneficiaryAcctNum"=bAcctNum, "beneficiaryAddress"=bAddr, "Beneficiary"=bnf)
    }
    return(v)
  })

  vars <- t(vars)

  orig <- vars[, 1]
  oAddr <- vars[, 2]
  oAcctNum <- vars[, 3]
  oBank <- vars[, 4]
  iBank <- vars[, 5]
  bBank <- vars[, 6]
  bAcctNum <- vars[, 7]
  bAddr <- vars[, 8]
  bnf <- vars[, 9]

  dat <- data.frame("Date"=date, "Amount"=amount, "Currency"=cur,
                    "Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
                    "originatorBank"=oBank, "intermediaryBank"=iBank,
                    "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
                    "beneficiaryAddress"=bAddr, "Beneficiary"=bnf, "Memo"=memo,
                    stringsAsFactors=F)

  dat %<>% clean_entities()
  return(dat)
}

parse_jpmc <- function(file) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (!tolower(format) %in% c("csv", "xls", "xlsx")) stop("This function expects ",
                                                          "an Excel file. ",
                                                          "Please use an ",
                                                          "appropriate file.")

  #Define parsing function
  findFlow <- function(row) {

    #common data
    date <- row['PaymentDate'] %>% as.Date(format="%m/%d/%Y")
    amount <- row['Amount'] %>% str_replace_all("[\\$,]", "")
    amount <- ifelse(grepl("\\.", amount), amount, paste(amount, "00", sep=".")) %>%
      str_replace_all("[\\$,]", "")
    #It's simply assumed that the currency is in dollars. This might not be right.
    cur <- "USD"
    memo <- paste(row['DetPymt1'], row['DetPymt2'],
                  row['DetPymt3'], row['DetPymt4'], sep=" ") %>%
      str_replace_all(" NA", "")

    #originator/beneficiary data
    #There are edge cases in which the entity is not found in the OrdCust2 field
    #It is sometimes in the field to the left, so need to check that the field to
    #the left is an account number and not an entity name. To do this, check the
    #ratio of numbers to letters in the string. If the entity is in the field to
    #the left, then the OrdCust1 is often a continuation of that entity's name.
    #In addition to this issue, OrdCust1 can also be blank, which means the
    #Originator has an account with JPMC and can be found in the DrAddr1 field.
    if (is.na(row['OrdCust1'])) {
      orig <- row['DrAddr1'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      oAcctNum <- row['DrId']
      oAddr <- paste(row['DrAddr2'], row['DrAddr3'], row['DrAddr4'], sep=" ") %>%
        gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all(" Na| Null", "")
    } else {
      numberCount <- row['OrdCust1'] %>% str_match_all("\\d") %>% .[[1]] %>% length()
      ratio <- numberCount / nchar(row['OrdCust1'])
      if (ratio > 0.60) {
        orig <- row['OrdCust2'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        oAcctNum <- row['OrdCust1']
      } else {
        orig <- paste(row['OrdCust1'], row['OrdCust2'], sep="") %>%
          gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        oAcctNum <- NA
      }
      oAddr <- paste(row['OrdCust3'], row['OrdCust4'], collapse=" ") %>%
        gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all(" Na| Null", "")
    }
    #The Beneficiary can be in one of two fields: AcctPty or UltBene. The
    #Beneficiary is in the UltBene field if the AcctPty field is a bank. In this
    #situation, that most likely means there is a second intermediary bank.
    #So let's introduce some logic to check for that. In addition, the same issue
    #that was mentioned above with Originator can happen here, so check for that
    #as well.
    if (is.na(row['UltBene2'])) {
      numberCount <- row['AcctPty1'] %>% str_match_all("\\d") %>% .[[1]] %>% length()
      ratio <- numberCount / nchar(row['AcctPty1'])
      if (ratio > 0.60) {
        bnf <- row['AcctPty2'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        bAcctNum <- row['AcctPty1']
      } else {
        bnf <- paste(row['AcctPty1'], row['AcctPty2'], sep="") %>%
          gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        bAcctNum <- NA
      }
      bAddr <- paste(row['AcctPty3'], row['AcctPty4'], row['AcctyPty5'], sep=" ") %>%
        gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all(" Na| Null", "")
    } else {
      numberCount <- row['UltBene1'] %>% str_match_all("\\d") %>% .[[1]] %>% length()
      ratio <- numberCount / nchar(row['UltBene1'])
      if (ratio > 0.60) {
        bnf <- row['UltBene2'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        bAcctNum <- row['UltBene1']
      } else {
        bnf <- paste(row['UltBene1'], row['UltBene2'], sep="") %>%
          gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        bAcctNum <- NA
      }
      bAddr <- paste(row['UltBene3'], row['UltBene4'], row['UltBene5'], sep=" ") %>%
        gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all(" Na| Null", "")
    }

    #bank info
    #The bank info gets a bit complicated with JPMC, but essentially the DrAddr
    #and CrAddr fields are who JPMC debited and credited, respectively. As a result,
    #the DrAddr and CrAddr fields are almost always banks, but can be entities if those
    #entities have accounts at JPMC. Aside from that, if an Originator's bank does not
    #have a relationship with JPMC, it will go through another bank, which will then be
    #listed in the OrdBank field. Same with the Beneficiary's bank, except that bank will
    #be listed in the AcctPty field. To ensure accuracy, start with the Originator and
    #work through the flow of funds to get all banks.
    if (is.na(row['OrdCust2'])) {
      oBank <- "JPMorgan Chase"
    } else {
      if (is.na(row['OrdBank1'])) {
        oBank <- row['DrAddr1'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      } else {
        oBank <- row['OrdBank1'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        iBank <- row['DrAddr1'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      }
    }
    if (is.na(row['AcctPty2']) & is.na(row['UltBene2'])) {
      bBank <- "JPMorgan Chase"
    } else if (any(is.na(row['AcctPty2']), is.na(row['UltBene2']))) {
      bBank <- row['CrAddr1'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    } else {
      bBank <- row['AcctPty2'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      if (!exists("iBank")) {
        iBank <- row['CrAddr1'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      } else {
        ib2 <- row['CrAddr1'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        iBank <- paste(iBank, "JPMorgan Chase", ib2, sep=", ")
      }
    }
    if (!exists("iBank")) {
      iBank <- "JPMorgan Chase"
    } else {
      if (!grepl("JPMorgan Chase", iBank)) iBank %<>% c(., "JPMorgan Chase")
    }

    dataRow <- data.frame("Date"=date, "Amount"=amount, "Currency"=cur,
                          "Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
                          "originatorBank"=oBank, "intermediaryBank"=iBank,
                          "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
                          "beneficiaryAddress"=bAddr, "Beneficiary"=bnf, "Memo"=memo,
                          stringsAsFactors=F)
  }

  if (format %in% c("xls", "xlsx")) {
    functionName <- paste("readxl", format, "sheets", sep="_")
    sheetNames <- .Call(functionName, PACKAGE="readxl", file)
    for (sheetName in sheetNames) {
      tmp <- read_excel(file, sheet=sheetName)
      #cleanup
      names(tmp) %<>% str_replace_all("_", " ") %>%
        gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all(" ", "")
      tmp$OrdCust1 %<>% str_replace_all("^/", "")
      tmp$AcctPty1 %<>% str_replace_all("^/", "")
      tmp$CrId %<>% str_replace_all("^/", "")
      tmp$DrId %<>% str_replace_all("^/", "")
      tmp$UltBene1 %<>% str_replace_all("^/", "")
      dat_m <- do.call(rbind, apply(tmp, 1, function(row) findFlow(row)))
    }
  } else {
    tmp <- read_csv(file)
    names(tmp) %<>% str_replace_all("_", " ") %>%
        gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all(" ", "")
    tmp$OrdCust1 %<>% str_replace_all("^/", "")
    tmp$AcctPty1 %<>% str_replace_all("^/", "")
    tmp$CrId %<>% str_replace_all("^/", "")
    tmp$DrId %<>% str_replace_all("^/", "")
    tmp$UltBene1 %<>% str_replace_all("^/", "")
    dat_m <- do.call(rbind, apply(tmp, 1, findFlow()))
  }

  dat %<>% clean_entities()
  return(dat)
}

parse_ubs <- function(file) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (tolower(format) != "csv") stop("This function expects a CSV file. ",
                                     "If you're trying to load an Excel file, ",
                                     "Please save it as a CSV and reload it.")
  #The UBS data files can be very messy at best, so the reader tends to find issues
  #with the data and might not incorporate everything properly. As a result, we want
  #to read everything in as a character column so it doesn't try to do any formatting.
  #However, we first need to know how many columns there are.
  n <- python.call("get_number_of_columns", file)
  #Set a character vectors of "c"s to tell it to load all columns as character
  colTypes <- rep("c", cols) %>% paste(., collapse="")
  tmp <- read_csv(file, col_types=colTypes)
  #If UBS inserts hard returns in their column names then it messes up the reader
  #and the column names are included as a row in the data.
  check <- (names(tmp) %in% tmp[1, ]) %>% sum()
  if (check >= 2) tmp <- tmp[2:nrow(tmp), ]
  names(tmp) <- c("searchValue", "Date", "Beneficiary", "orderParty", "messageType",
                  "currencyCode", "Amount", "debitID", "debitName", "sendingBank",
                  "orderingBank", "creditName", "intermediaryBank", "beneficiaryBank",
                  "OBI", "BBI", "orderPartyRef", "orderPartyAddres", "creditAdvice",
                  "creditID", "coverOrderPartyAddress", "coverOrderParty",
                  "coverOrderingBank", "coverIntermediaryBank", "coverBeneficiaryBank",
                  "coverBeneficiary", "coverOBI", "coverRemittance", "InternalNDX")

  #common info
  date <- tmp$Date
  amount <- tmp$Amount
  cur <- tmp$currencyCode
  memo <- tmp$OBI

  #originator/beneficiary info
  orig <- "placeholder"
  oAddr <- "placeholder"
  oAcctNum <- "placeholder"
  bnf <- tmp$Beneficiary %>% str_replace_all("^[A-Z]*[0-9\\-]+(?=[A-Z]+)", "")
  bAddr <- "placeholder"
  bAcctNum <- "placeholder"

  return(tmp)

#   dat %<>% clean_entities()
#   return(dat)
}

parse_tdbank <- function(file) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (tolower(format) != "pdf") stop("This function expects a PDF file. ",
                                     "Please use an appropriate file.")
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

    #common data
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
    cur <- str_extract(textBlock[CIDX], "(?<=Currency\\s{1,10}).*")
    #common OCR mistake is confusing "USD" with "USO", so replace that
    cur[cur == "USO"] <- "USD"
    MIDX <- grep("^OBI\\s+", textBlock)[1]
    memo <- str_extract(textBlock[MIDX], "(?<=OBI\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    #originator/beneficiary info
    OIDX <- grep("^Originator", textBlock, ignore.case=T)[1]
    orig <- str_extract(textBlock[OIDX], "(?<=Originator\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    OAIDX <- grep("ORG ADDR", textBlock)[1]
    oAddr <- paste(textBlock[OAIDX:(OAIDX+2)], collapse=" ") %>%
      str_replace_all("ORG ADDR\\d", "")
    #Because BNF ID matches the account number, I assume ORG ID is the
    #Originator Account Number
    OANIDX <- grep("^ORG ID", textBlock, ignore.case=T)[1]
    oAcctNum <- str_extract(textBlock[OANIDX], "(?<=ORG ID\\s{1,10}).*")

    BIDX <- grep("^Beneficiary", textBlock, ignore.case=T)[1]
    bnf <- str_extract(textBlock[BIDX], "(?<=Beneficiary\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    BAIDX <- grep("^BNF ADDR", textBlock)[1]
    bAddr <- paste(textBlock[BAIDX:(BAIDX+2)], collapse=" ") %>%
      str_replace_all("BNF ADDR\\d", "")
    BANIDX <- grep("^BNF ID", textBlock)[1]
    bAcctNum <- str_extract(textBlock[BANIDX], "(?<=BNF ID\\s{1,10}).*")

    #bank info
    #There is no "Orig Bank" field, but Sender Name appears to be the
    #field we're looking for.
    OBIDX <- grep("^Sender Name", textBlock, ignore.case=T)[1]
    oBank <- str_extract(textBlock[OBIDX], "(?<=Sender Name\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    BBIDX <- grep("^Bene Bank", textBlock, ignore.case=T)[1]
    bBank <- str_extract(textBlock[BBIDX], "(?<=Bene Bank\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    IBIDX <- grep("^Intermd Bank", textBlock, ignore.case=T)[1]
    iBank <- str_extract(textBlock[IBIDX], "(?<=Intermd Bank\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)

    #There is a fourth bank field called Recv Name. This field appears to never
    #be blank, but Bene Bank and Intermd Bank can be, so need to set this field
    #and then run some logic.
    RBIDX <- grep("^Recv Name", textBlock, ignore.case=T)[1]
    rBank <- str_extract(textBlock[RBIDX], "(?<=Recv Name\\s{1,10}).*") %>%
      gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
    #There are two possibilities: 1) Recv Bank is the Intermediary Bank or
    # 2) it's the Beneficiary Bank. Because Recv Bank appears to never be empty,
    #I'm assuming that it is the actual beneficiary bank.
    if (is.na(bBank)) {
      bBank <- rBank
    } else {
      if (is.na(iBank)) {
        iBank <- bBank
        bBank <- rBank
      } else {
        iBank <- paste(iBank, bBank, sep=", ")
        bBank <- rBank
      }
    }

    return(c("Date"=date, "Amount"=amount, "Currency"=cur, "Originator"=orig,
             "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediaryBank"=iBank,
             "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
             "beneficiaryAddress"=bAddr, "Beneficiary"=bnf, "Memo"=memo))
  }))
  dat <- dat_m %>% as.data.frame(stringsAsFactors=F)
  dat %<>% clean_entities()
  return(dat)
}

