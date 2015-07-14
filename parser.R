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
  data$Originator %<>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    str_trim() %>%
    str_replace_all("(?<=Llc\\b|Inc\\b|Ltd\\b|Co\\b).*", "") %>%
    str_replace_all("\\(.*\\)", "") %>%
    str_replace_all("[\\.,]", "") %>%
    gsub("\\b([A-z]{2,3})$", "\\U\\1", ., perl=T) %>%
    gsub("^([A-z]{2,3})\\b", "\\U\\1", ., perl=T) %>%
    str_replace_all("\\bCO$", "Corporation") %>%
    str_replace_all("\\bLTD$|\\bLT$", "Limited") %>%
    str_replace_all("\\bINC$", "Incorporated") %>%
    str_replace_all("\\bPty\\b", "Proprietary") %>%
    str_replace_all("^\\d/", "") %>%
    str_replace_all("(?<=\\w)\\d/(?=\\w)", ", ") %>%
    str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "")
  data$Beneficiary %<>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    str_trim() %>%
    str_replace_all("(?<=Llc\\b|Inc\\b|Ltd\\b|Co\\b).*", "") %>%
    str_replace_all("\\(.*\\)", "") %>%
    str_replace_all("[\\.,]", "") %>%
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
    str_trim() %>%
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
    str_trim() %>%
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
    str_trim() %>%
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
  data$Memo %<>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>% str_trim()
  data$originatorAddress %<>% str_replace_all("(\\w,)(\\w)", "\\1 \\2") %>%
    gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>% str_trim()
  data$beneficiaryAddress %<>% str_replace_all("(\\w,)(\\w)", "\\1 \\2") %>%
    gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T) %>% str_trim()

  return(data)
}

load_excel <- function(file, sheet=1, skip=0, colNames=T) {
  format <- file_ext(file) %>% tolower()
  if (format == "csv") {
    n <- read_csv(file, n_max=1) %>% names() %>% length()
    colTypes <- rep("c", n) %>% paste(., collapse='')
    tmp <- read_csv(file, col_types=colTypes, col_names=colNames, skip=skipNumber)
  } else {
    #check for sheet name in sheet names
    sheetNames <- excel_sheets(file)
    if (length(sheetNames) > 1) {
      if (is.numeric(sheet)) sheet <- sheetNames[sheet]
      while (!sheet %in% sheetNames) {
        sheet <- readline(prompt="What is the name of the sheet with the data? ")
      }
      sheetIndex <- which(sheetNames == sheet)
    } else {
      sheetIndex <- 1
    }
    if (colNames) {
      #get number of columns and create a vector of column types
      functionName <- paste("readxl", format, "col_names", sep="_")
      #C++ starts counting at 0, not 1
      colNames <- .Call(functionName, PACKAGE = 'readxl', file, sheetIndex - 1, skip)
      n <- length(colNames)
      colTypes <- rep("text", n)
      tmp <- read_excel(file, col_types=colTypes, sheet=sheetIndex, skip=skip)
    } else {
      #This is for files like those from Bank of New York Mellon
      tmp <- read_excel(file, sheet=sheetIndex, col_names=F)
    }
  }
  tmp
}

standardize_names <- function(data) {
  names(data) %<>% str_replace_all("_", " ") %>%
    str_trim() %>%
    gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    str_replace_all("\\s?/\\s?", " ") %>%
    str_replace_all(" ", "") %>%
    gsub("^([A-Z])(?=[a-z]+[A-Z])", "\\L\\1", ., perl=T)
  data
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
    memo <- textBlock[MIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2]

    #BoA Wires have a different format depending on whether a customer received a wire or sent one.
    #If a BoA customer *sends* a wire, it will say "**** CREDIT PAYMENT MESSAGE TEXT ****" on the wire.
    if (any(grepl("CREDIT PAYMENT MESSAGE TEXT", textBlock))) {
      s <- grep("CREDIT PAYMENT MESSAGE TEXT", textBlock)
      if (any(grepl("SWIFT Message Text", textBlock))) f <- grep("SWIFT Message Text", textBlock) else f <- length(textBlock)
      messageBlock <- textBlock[s:f]

      OBIDX <- grep("Sending Bank:", messageBlock, ignore.case=T)[1] + 3
      oBank <- messageBlock[OBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        str_split(., "\\s{2,}") %>% unlist() %>% .[2]
      BBIDX <- grep("Receiving Bank:", messageBlock, ignore.case=T)[1] + 3
      bBank <- messageBlock[BBIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        str_split(., "\\s{2,}") %>% unlist() %>% .[2]
      if (any(grepl("Beneficiary's Bank", messageBlock, ignore.case=T))) {
        iBank <- bBank
        BBIDX <- grep("Beneficiary's Bank", messageBlock, ignore.case=T)[1] + 1
        bBank <- messageBlock[BBIDX]
      } else {
        iBank <- NA
      }
      BIDX <- grep("Beneficiary:", messageBlock, ignore.case=T)[1] + 1
      bnf <- messageBlock[BIDX]
      bAddr <- textBlock[(BIDX+1):(BIDX+2)] %>% paste(., collapse=" ")
      OIDX <- grep("Originator:", messageBlock, ignore.case=T) + 1
      orig <- messageBlock[OIDX]
      oAddr <- textBlock[(OIDX+1):(OIDX+2)] %>% paste(., collapse=" ")
    } else {
      bBank <- "Bank of America"
      OBIDX <- grep("DEBIT VAL:\\s?../../..", textBlock, ignore.case=T)[1] + 1
      bnf <- textBlock[OBIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[2]
      bAddr <- textBlock[(OBIDX+1):(OBIDX+2)] %>% str_split(., "\\s{3,}") %>%
        unlist() %>% .[c(2, 4)] %>% paste(., collapse=" ")
      OIDX <- grep("ORIG:\\s*/\\s*[A-Z0-9]+", textBlock, ignore.case=T)[1] + 1
      orig <- textBlock[OIDX] %>% gsub("^\\s+|\\s+$", "", .) %>%
        str_split(., "\\s{2,}") %>% unlist() %>% .[1]
      oAddr <- textBlock[(OIDX+1):(OIDX+2)] %>%
        str_split(., "\\s{3,}") %>% unlist()
      if (length(oAddr) > 2) {
        oAddr %<>% .[c(1, 3)] %>% paste(., collapse=" ")
      } else {
        oAddr %<>% paste(., collapse=" ")
      }
      oBank <- textBlock[OBIDX] %>% str_split(., "\\s{2,}") %>% unlist() %>% .[1]
      if (any(grepl("ORDERING BNK:", textBlock, ignore.case=T))) {
        iBank <- oBank
        OBIDX <- grep("ORDERING BNK:", textBlock, ignore.case=T)[1] + 1
        oBank <- textBlock[OBIDX]
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

parse_bny <- function(file, skip=0, sheet=1) {
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
    memo <- str_extract(textBlock[MIDX], "(?<=OBI\\s{1,10}).*")

    #originator/beneficiary info
    OIDX <- grep("^Originator", textBlock, ignore.case=T)[1]
    orig <- str_extract(textBlock[OIDX], "(?<=Originator\\s{1,10}).*")
    OAIDX <- grep("ORG ADDR", textBlock)[1]
    oAddr <- paste(textBlock[OAIDX:(OAIDX+2)], collapse=" ") %>%
      str_replace_all("ORG ADDR\\d", "")
    #Because BNF ID matches the account number, I assume ORG ID is the
    #Originator Account Number
    OANIDX <- grep("^ORG ID", textBlock, ignore.case=T)[1]
    oAcctNum <- str_extract(textBlock[OANIDX], "(?<=ORG ID\\s{1,10}).*")

    BIDX <- grep("^Beneficiary", textBlock, ignore.case=T)[1]
    bnf <- str_extract(textBlock[BIDX], "(?<=Beneficiary\\s{1,10}).*")
    BAIDX <- grep("^BNF ADDR", textBlock)[1]
    bAddr <- paste(textBlock[BAIDX:(BAIDX+2)], collapse=" ") %>%
      str_replace_all("BNF ADDR\\d", "")
    BANIDX <- grep("^BNF ID", textBlock)[1]
    bAcctNum <- str_extract(textBlock[BANIDX], "(?<=BNF ID\\s{1,10}).*")

    #bank info
    #There is no "Orig Bank" field, but Sender Name appears to be the
    #field we're looking for.
    OBIDX <- grep("^Sender Name", textBlock, ignore.case=T)[1]
    oBank <- str_extract(textBlock[OBIDX], "(?<=Sender Name\\s{1,10}).*")
    BBIDX <- grep("^Bene Bank", textBlock, ignore.case=T)[1]
    bBank <- str_extract(textBlock[BBIDX], "(?<=Bene Bank\\s{1,10}).*")
    IBIDX <- grep("^Intermd Bank", textBlock, ignore.case=T)[1]
    iBank <- str_extract(textBlock[IBIDX], "(?<=Intermd Bank\\s{1,10}).*")

    #There is a fourth bank field called Recv Name. This field appears to never
    #be blank, but Bene Bank and Intermd Bank can be, so need to set this field
    #and then run some logic.
    RBIDX <- grep("^Recv Name", textBlock, ignore.case=T)[1]
    rBank <- str_extract(textBlock[RBIDX], "(?<=Recv Name\\s{1,10}).*")
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

parse_citibank <- function(file, skip=1, sheet=1) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (!tolower(format) %in% c("csv", "xls", "xlsx", "doc", "docx")) {
    stop("This function expects an Excel or Word file. Please use an appropriate file.")
  }
  if (format %in% c("xls", "xlsx", "csv")) {
    #load the file and clean it up
    tmp <- load_excel(file, skip=skip, sheet=sheet)
    if (ncol(tmp) != 12) tmp <- tmp[, 1:12]
    #For now, I'm going to make a bold assumption that the variable names will stay the
    #same. In the future, I'll need to figure out a way to dynamically identify the
    #appropriate names. But that's probably true for all of these files...
    names(tmp) <- c("globalID", "instructionDate", "Originator", "originatorBank",
                    "dbOrInterParty", "debitedParty", "creditedParty", "crOrInterParty",
                    "beneficiaryBank", "Beneficiary","Amount", "OBI")
    tmp <- apply(tmp, 2, function(z) ifelse(z %in% c("()", " "), NA, z)) %>%
      as.data.frame(stringsAsFactors=F)
    tmp %<>% filter(rowSums(is.na(.)) != ncol(.))

    #common data
    date <- tmp$instructionDate %>% as.numeric() %>% as.Date(origin="1899-12-30")
    amount <- tmp$Amount %>% as.numeric() %>% round(., 2)
    cur <- "USD"
    memo <- tmp$OBI

    vars <- apply(tmp, 1, function(row) {
      #originator/beneficiary info
      orig <- row['Originator'] %>% str_replace("\\s+\\(.*\\)", "")
      oAddr <- NA
      oAcctNum <- row['Originator'] %>% str_extract("(?<=\\s{1,10})\\(.*\\)") %>%
        str_replace_all("\\(|\\)", "")
      bnf <- row['Beneficiary'] %>% str_replace("\\s+\\(.*\\)", "")
      bAddr <- NA
      bAcctNum <- row['Beneficiary'] %>% str_extract("(?<=\\s{1,10})\\(.*\\)") %>%
        str_replace_all("\\(|\\)", "")

      #bank info
      if (is.na(row['originatorBank'])) {
        oBank <- "Citibank"
      } else {
        oBankValues <- row['originatorBank'] %>% str_split("\\(") %>%
          .[[1]] %>% str_replace_all("[\\s{2,}\\)]", "")
        swiftFlag <- oBankValues[1] == oBankValues[2]
        if (swiftFlag) {
          oBank <- row['dbOrInterParty']
        } else {
          oBank <- row['originatorBank']
        }
      }
      oBank %<>% str_replace_all("\\(.*\\)|\\s{2,}", "")

      if (is.na(row['beneficiaryBank'])) {
        bBank <- "Citibank"
      } else {
        bBankValues <- row['beneficiaryBank'] %>% str_split("\\(") %>%
          .[[1]] %>% str_replace_all("[\\s{2,}\\)]", "")
        swiftFlag <- bBankValues[1] == bBankValues[2]
        if (swiftFlag) {
          bBank <- row['crOrInterParty']
        } else {
          bBank <- row['beneficiaryBank']
        }
      }
      bBank %<>% str_replace_all("\\(.*\\)|\\s{2,}", "")

      #Account for when DB/CR or Inter Party is different from the Orig/Benef Bank
      #and the Orig/Benef bank is not a SWIFT code
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
          if (!swiftFlag) dbi <- dbiValue else dbi <- NA
        } else {
          dbi <- NA
        }
      } else {
        dbi <- NA
      }
      dbi %<>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      if (!is.na(row['crOrInterParty'])) {
        criValue <- row['crOrInterParty'] %>% str_replace_all("\\(.*\\)", "") %>%
          str_replace_all("^\\s+|\\s+$", "") %>% tolower()
        beneBankValue <- row['beneficiaryBank'] %>% str_replace_all("\\(.*\\)", "") %>%
          str_replace_all("^\\s+|\\s+$", "") %>% tolower()
        if (stringdist(criValue, beneBankValue)/nchar(criValue) > 1/3) {
          bBankValues <- row['beneficiaryBank'] %>% str_split("\\(") %>%
            .[[1]] %>% str_replace_all("[\\s{2,}\\)]", "")
          swiftFlag <- bBankValues[1] == bBankValues[2]
          if (!swiftFlag) cri <- criValue else cri <- NA
        } else {
          cri <- NA
        }
      } else {
        cri <- NA
      }
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
      v <- c("Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediateBank"=iBank, "beneficiaryBank"=bBank,
             "beneficiaryAcctNum"=bAcctNum, "beneficiaryAddress"=bAddr, "Beneficiary"=bnf)
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

parse_hsbc <- function(file, skip = 0, sheet="Details") {
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
  #Becuase we're reading the data in as character vales, it'll be read as the number of days
  #since epoch. Excel is a bit dumb though and treated 1900 as a leap year when it wasn't.
  #So need to set the origin back a couple days. Some tweaking will need to be done if I want
  #to make this usable with Mac spreadsheet files.
  date <- tmp$transactionDate %>% as.numeric %>% as.Date(origin="1899-12-30")
  amount <- tmp$Amount %>% as.numeric() %>% round(., 2)
  amount <- ifelse(grepl("\\.", amount), amount, paste(amount, "00", sep="."))
  cur <- tmp$ccyCodeCurrency

  #If the Beneficiary listed is a bank, HSBC appends data in *Seqb fields.
  #It's possible that the Originator is also listed as a bank, which, to the
  #best of my knowledge, only occurs if the Beneficiary is a bank. This can be
  #determined by comparing the Originator to the OriginatorSeqb field. Either
  #way, I need to assign variables based on whether or not the Beneficiary is
  #a bank. The returned value will be a matrix that will need to be transposed.
  vars <- apply(tmp, 1, function(row) {
    #memo
    if (is.na(row['senderBankCorresp'])) {
      if (is.na(row['receiverBankCorresp'])) {
        memo <- NA
      } else {
        memo <- paste("Recieving Bank:", row['receiverBankCorresp'])
      }
    } else {
      if (is.na(row['recieverBankCorresp'])) {
        memo <- paste("Sending Bank:", row['senderBankCorresp'])
      } else {
        memo <- paste("Sending Bank:", row['senderBankCorresp'],
                     "Recieving Bank:", row['receiverBankCorresp'])
      }
    }

    #If there's no *Seqb variables, assign as usual
    if (is.na(row['originatorSeqb'])) {
      #originator/beneficiary info
      origField <- row['Originator'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
      orig <- origField[2]
      oAcctNum <- origField[1]
      if (length(origField) >= 3) {
        oAddr <- origField %>% .[3:length(.)] %>%
          str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
          .[nchar(.)> 3] %>% paste(., collapse=" ")
      } else {
        oAddr <- NA
      }
      bnfField <- row['Beneficiary'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
      bnf <- bnfField[2]
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
        oBank <- row['DebitParty']
      } else {
        #Sometimes only the SWIFT code is included in a Bank field so the bank comes
        #back as NA. In this case, set it as the SWIFT code and will replace later.
        oBank <- row['originatorBank'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2]
        if (is.na(oBank)) oBank <- row['originatorBank'] %>% str_split("\\s{3}") %>%
          .[[1]] %>% .[1]
      }
      #BeneficiaryBank is always empty, so set it to Credit Party.
      #Credit Party has been blank in some instances. In this case, set it to "Unknown".
      bBank <- row['creditParty']
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
          iBank <- row['debitParty']
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
        orig <- origField[2]
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
          oBank <- row['debitParty']
        } else {
          oBank <- row['originatorBank'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2]
          if (is.na(oBank)) oBank <- row['originatorBank'] %>% str_split("\\s{3}") %>%
            .[[1]] %>% .[1]
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
            iBank <- cpValue
          }
        } else {
          dpValue <- row['debitParty']
          cpValue <- row['creditParty']
          obValue <- row['originatorBank']
          bbValue <- row['Beneficiary']
          if (grepl(dpValue, obValue, fixed=T) & grepl(cpValue, bbValue, fixed=T)) {
            iBank <- NA
          } else if (grepl(dpValue, obValue, fixed=T) & !grepl(cpValue, bbValue, fixed=T)) {
            iBank <- cpValue
          } else if (!grepl(dpValue, obValue, fixed=T) & grepl(cpValue, bbValue, fixed=T)) {
            iBank <- dpValue
          } else {
            iBank <- paste(dpValue, cpValue, sep=", ")
          }
        }
      } else {
        origField <- row['originatorSeqb'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
        orig <- origField[2]
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
          oBank <- row['Originator'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2]
          if (is.na(oBank)) oBank <- row['originatorBank'] %>% str_split("\\s{3}") %>%
              .[[1]] %>% .[1] %>% str_replace_all("^\\s+|\\s+$", "")
        }
        iBank <- row['debitParty']
      }
      #Beneficiary Bank should be in the Beneficiary field and the Beneficiary should
      #be in the BeneficiarySeqb field.
      bnfField <- row['beneficiarySeqb'] %>% str_split("\\s{3,}|(?<=\\d{5,30})/(?=[A-Z]+)") %>% .[[1]]
      bnf <- bnfField[2]
      bAcctNum <- bnfField[1]
      if (length(bnfField) >= 3) {
        bAddr <- bnfField %>% .[3:length(.)] %>%
          str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
          .[nchar(.)> 3] %>% paste(., collapse=" ")
      } else {
        bAddr <- NA
      }
      bBank <- row['Beneficiary'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2]
      if (is.na(bBank)) bBank <- row['Beneficiary'] %>% str_split("\\s{3}") %>%
        .[[1]] %>% .[1] %>% str_replace_all("^\\s+|\\s+$", "")

      v <- c("Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediateBank"=iBank, "beneficiaryBank"=bBank,
             "beneficiaryAcctNum"=bAcctNum, "beneficiaryAddress"=bAddr,
             "Beneficiary"=bnf, "memo"=memo)
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
  memo <- vars[, 10]

  dat <- data.frame("Date"=date, "Amount"=amount, "Currency"=cur,
                    "Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
                    "originatorBank"=oBank, "intermediaryBank"=iBank,
                    "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
                    "beneficiaryAddress"=bAddr, "Beneficiary"=bnf, "Memo"=memo,
                    stringsAsFactors=F)

  dat %<>% clean_entities()
  return(dat)
}

parse_jpmc <- function(file, skip=0, sheet=1) {
  format <- file_ext(file) %>% tolower()
  if (!file.exists(file)) stop("Invalid file path. ",
                               "Please be sure to use the full ",
                               "file path to a valid file.")
  if (!tolower(format) %in% c("csv", "xls", "xlsx")) stop("This function expects ",
                                                          "an Excel file. ",
                                                          "Please use an ",
                                                          "appropriate file.")
  tmp <- load_excel(file, sheet=sheet) %>% standardize_names()

  #common data
  date <- tmp$paymentDate %>% as.numeric() %>% as.Date(origin="1899-12-30")
  amount <- tmp$Amount %>% as.numeric() %>% round(., 2)
  amount <- ifelse(grepl("\\.", amount), amount, paste(amount, "00", sep=".")) %>%
    str_replace_all(",", "")
  #It's simply assumed that the currency is in dollars. This might not be right.
  cur <- "USD"

  vars <- apply(tmp, 1, function(row) {
    memo <- paste(row['detPymt1'], row['detPymt2'], row['detPymt3'], row['detPymt4'], sep=" ") %>%
      str_replace_all(" NA| NULL", "")

    #originator/beneficiary info
    #There are scenarios in which the originator or the beneficiary is a customer
    #of JPMC directly and as a result will not be listed in ordCust2, but instead
    #will be listed in drAddr1.In addition, it is possible that in the ordCust and
    #acctPty fields that the entity names spill over from the second column to the
    #first (i.e. ordCust2 spills over into ordCust1). So if the originator or
    #beneficiary need to be set to these values, we must first check for any spillover.
    if (is.na(row['ordCust2'])) {
      orig <- row['drAddr1']
      oAcctNum <- row['drId']
      oAddr <- paste(row['drAddr2'], row['drAddr3'], row['drAddr4'], collapse=" ") %>%
        str_replace_all(" NA| NULL", "")
    } else {
      numberOfDigits <- row['ordCust1'] %>% str_extract_all("[0-9]") %>% .[[1]] %>% length()
      isAccountNumber <- if (numberOfDigits / nchar(row['ordCust1']) > 0.60) T else F
      if (isAccountNumber) {
        orig <- row['ordCust2'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        oAcctNum <- row['ordCust1']
        oAddr <- paste(row['ordCust3'], row['ordcust4'], collapse=" ") %>%
          str_replace_all(" NA| NULL", "")
      } else {
        orig <- paste(row['ordCust1'], row['ordCust2'], collapse="")
        oAcctNum <- NA
        oAddr <- paste(row['ordCust3'], row['ordcust4'], collapse=" ") %>%
          str_replace_all(" NA| NULL", "")
      }
    }

    #For the beneficiary, the acctPty2 variable can be empty while the utlBene2
    #field is populated. So need to check both.
    if (is.na(row['acctPty2']) && is.na(row['ultBene2'])) {
      bnf <- row['crAddr1']
      bAcctNum <- row['crId']
      bAddr <- paste(row['crAddr2'], row['crAddr3'], row['crAddr4'], collapse=" ") %>%
        str_replace_all(" NA| NULL", "")
    } else if (is.na(row['ultBene2'])) {
      numberOfDigits <- row['acctPty1'] %>% str_extract_all("[0-9]") %>% .[[1]] %>% length()
      isAccountNumber <- if (numberOfDigits / nchar(row['acctPty1']) > 0.60) T else F
      if (isAccountNumber) {
        bnf <- row['acctPty2'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        bAcctNum <- row['acctPty1']
      } else {
        bnf <- paste(row['acctPty1'], row['acctPty2'], collapse="")
        bAcctNum <- NA
      }
      bAddr <- paste(row['acctPty3'], row['acctPty4'], row['acctPty5'], collapse=" ") %>%
        str_replace_all(" NA| NULL", "")
    } else {
      numberOfDigits <- row['ultBene1'] %>% str_extract_all("[0-9]") %>% .[[1]] %>% length()
      isAccountNumber <- if (numberOfDigits / nchar(row['ultBene1']) > 0.60) T else F
      if (isAccountNumber) {
        bnf <- row['ultBene2'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
        bAcctNum <- row['ultBene1']
      } else {
        bnf <- paste(row['ultBene1'], row['ultBene2'], collapse="")
        bAcctNum <- NA
      }
      bAddr <- paste(row['ultBene3'], row['ultBene4'], row['ultBene5'], collapse=" ") %>%
        str_replace_all(" NA| NULL", "")
    }

    #bank info
    #If the Originator is set to crId then Originator Bank should be JPMC. Same with the
    #beneficiary. From there, work from Originator to Beneficiary to determine the
    #Intermediary Bank.
    if (is.na(row['ordCust2'])) {
      oBank <- "JPMorgan Chase"
    } else if (is.na(row['ordBank1'])) {
      oBank <- row['drAddr1']
    } else {
      oBank <- row['ordBank1']
      diBank <- row['drAddr1']
    }

    if (is.na(row['acctPty2']) && is.na(row['ultBene2'])) {
      bBank <- "JPMorgan Chase"
    } else if (!is.na(row['acctPty2']) && !is.na(row['ultBene2'])) {
      numberOfDigits <- row['acctPty1'] %>% str_extract_all("[0-9]") %>% .[[1]] %>% length()
      isAccountNumber <- if (numberOfDigits / nchar(row['acctPty2']) > 0.60) T else F
      if (isAccountNumber) {
        bBank <- row['acctPty2'] %>% gsub("\\b([A-z])([A-z]+)", "\\U\\1\\L\\2", ., perl=T)
      } else {
        bBank <- paste(row['acctPty1'], row['acctPty2'], collapse="")
      }
      ciBank <- row['crAddr1']
    } else {
      bBank <- row['crAddr1']
    }

    if (exists("diBank") && exists("ciBank")) {
      if (oBank != "JPMorgan Chase" && bBank != "JPMorgan Chase") {
        iBank <- paste(diBank, "JPMorgan Chase", ciBank, sep=", ")
      } else {
        iBank <- paste(diBank, ciBank, sep=", ")
      }
    } else if (!exists("diBank") && !exists("ciBank")) {
      if (oBank != "JPMorgan Chase" && bBank != "JPMorgan Chase") {
        iBank <- "JPMorgan Chase"
      } else {
        iBank <- NA
      }
    } else if (!exists("diBank")) {
      iBank <- paste("JPMorgan Chase", ciBank, sep=", ")
    } else {
      iBank <- paste(diBank, "JPMorgan Chase", sep=", ")
    }

    v <- c("Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
           "originatorBank"=oBank, "intermediateBank"=iBank, "beneficiaryBank"=bBank,
           "beneficiaryAcctNum"=bAcctNum, "beneficiaryAddress"=bAddr,
           "Beneficiary"=bnf, "memo"=memo)

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
  memo <- vars[, 10]

  dat <- data.frame("Date"=date, "Amount"=amount, "Currency"=cur,
                    "Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
                    "originatorBank"=oBank, "intermediaryBank"=iBank,
                    "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
                    "beneficiaryAddress"=bAddr, "Beneficiary"=bnf, "Memo"=memo,
                    stringsAsFactors=F)

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
    memo <- str_extract(textBlock[MIDX], "(?<=OBI\\s{1,10}).*")

    #originator/beneficiary info
    OIDX <- grep("^Originator", textBlock, ignore.case=T)[1]
    orig <- str_extract(textBlock[OIDX], "(?<=Originator\\s{1,10}).*")
    OAIDX <- grep("ORG ADDR", textBlock)[1]
    oAddr <- paste(textBlock[OAIDX:(OAIDX+2)], collapse=" ") %>%
      str_replace_all("ORG ADDR\\d", "")
    #Because BNF ID matches the account number, I assume ORG ID is the
    #Originator Account Number
    OANIDX <- grep("^ORG ID", textBlock, ignore.case=T)[1]
    oAcctNum <- str_extract(textBlock[OANIDX], "(?<=ORG ID\\s{1,10}).*")

    BIDX <- grep("^Beneficiary", textBlock, ignore.case=T)[1]
    bnf <- str_extract(textBlock[BIDX], "(?<=Beneficiary\\s{1,10}).*")
    BAIDX <- grep("^BNF ADDR", textBlock)[1]
    bAddr <- paste(textBlock[BAIDX:(BAIDX+2)], collapse=" ") %>%
      str_replace_all("BNF ADDR\\d", "")
    BANIDX <- grep("^BNF ID", textBlock)[1]
    bAcctNum <- str_extract(textBlock[BANIDX], "(?<=BNF ID\\s{1,10}).*")

    #bank info
    #There is no "Orig Bank" field, but Sender Name appears to be the
    #field we're looking for.
    OBIDX <- grep("^Sender Name", textBlock, ignore.case=T)[1]
    oBank <- str_extract(textBlock[OBIDX], "(?<=Sender Name\\s{1,10}).*")

    BBIDX <- grep("^Bene Bank", textBlock, ignore.case=T)[1]
    bBank <- str_extract(textBlock[BBIDX], "(?<=Bene Bank\\s{1,10}).*")

    IBIDX <- grep("^Intermd Bank", textBlock, ignore.case=T)[1]
    iBank <- str_extract(textBlock[IBIDX], "(?<=Intermd Bank\\s{1,10}).*")

    #There is a fourth bank field called Recv Name. This field appears to never
    #be blank, but Bene Bank and Intermd Bank can be, so need to set this field
    #and then run some logic.
    RBIDX <- grep("^Recv Name", textBlock, ignore.case=T)[1]
    rBank <- str_extract(textBlock[RBIDX], "(?<=Recv Name\\s{1,10}).*")
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

