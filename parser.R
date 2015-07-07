library(readr)
library(readxl)
library(tools)
library(dplyr)
library(magrittr)
library(stringr)
library(stringdist)

#Ensure that large amounts of money are not recorded in scientific notation
options(scipen=999)

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
    BAIDX <- grep("^BNF ADDR", textBlock)[1]
    bAddr <- paste(textBlock[BAIDX:(BAIDX+2)], collapse=" ") %>%
      str_replace_all("BNF ADDR\\d", "")
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
    OAIDX <- grep("ORG ADDR", textBlock)[1]
    oAddr <- paste(textBlock[OAIDX:(OAIDX+2)], collapse=" ") %>%
      str_replace_all("ORG ADDR\\d", "")

    return(c("Date"=date, "Amount"=amount, "Currency"=cur, "Originator"=orig,
             "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediaryBank"=iBank,
             "beneficiaryBank"=bBank, "benficiaryAcctNum"=bAcctNum,
             "beneficiaryAddress"=bAddr, "Beneficiary"=bnf, "Memo"=memo))
  }))
  dat <- dat_m %>% as.data.frame(stringsAsFactors=F)
  return(dat)
}

parseCitibank <- function(file, n=1) {
  type = file_ext(file) %>% tolower()
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

    #common data
    date <- tmp$instructionDate
    amount <- tmp$amount
    cur <- "USD"
    memo <- tmp$OBI

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
          str_replace_all("^\\s+|\\s+$", "") %>% towlower()
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
  tmp <- read_excel(file, sheet=sheetName) %>% filter(rowSums(is.na(.)) != ncol(.))
  names(tmp) %<>% str_replace_all("_", " ") %>%
    gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
    str_replace_all(" ", "")

  #common data
  date <- tmp$TransactionDate %>% as.Date(format="%m/%d/%Y")
  amount <- tmp$Amount
  amount <- ifelse(grepl("\\.", amount), amount, paste(amount, "00", sep="."))
  cur <- tmp$CcyCodeCurrency
  memo <- apply(tmp, 1, function(row) {
    if (is.na(row['SenderBankCorresp'])) {
      if (is.na(row['ReceiverBankCorresp'])) {
        val <- NA
      } else {
        val <- paste("Recieving Bank:", row['ReceiverBankCorresp'])
      }
    } else {
      if (is.na(row['RecieverBankCorresp'])) {
        val <- paste("Sending Bank:", row['SenderBankCorresp'])
      } else {
        val <- paste("Sending Bank:", row['SenderBankCorresp'],
                     "Recieving Bank:", row['ReceiverBankCorresp'])
      }
    }
    val
  })

  #If the Beneficiary listed is a bank, HSBC appends data in *Seqb fields.
  #It's possible that the Originator is also listed as a bank, which, to the
  #best of my knowledge, only occurs if the Beneficiary is a bank. This can be
  #determined by comparing the Originator to the OriginatorSeqb field. Either
  #way, I need to assign variables based on whether or not the Beneficiary is
  #a bank. The returned value will be a list of named vectors that will need
  #to be separated into individual variables.
  vars <- apply(tmp, 1, function(row) {
    #If there's no *Seqb variables, assign as usual
    if (is.na(row['OriginatorSeqb'])) {
      #originator/beneficiary info
      origField <- row['Originator'] %>% str_split("\\s{3,}|(?<=\\d{1,30}/(?=[A-Z]+)") %>% .[[1]]
      orig <- origField[2] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
      oAcctNum <- origField[1]
      if (length(origField) >= 3) {
        oAddr <- origField %>% .[3:length(.)] %>%
          str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
          .[nchar(.)> 3] %>% paste(., collapse=" ")
      } else {
        oAddr <- NA
      }
      bnfField <- row['Beneficiary'] %>% str_split("\\s{3,}|(?<=\\d{1,30}/(?=[A-Z]+)") %>% .[[1]]
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
      oBank <- row['OriginatorBank'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2] %>%
        gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all("^\\s+|\\s+$", "")
      #Sometimes only the SWIFT code is included in the Originator Bank field so
      #oBank comes back as NA. In this case, set it as the SWIFT code and will replace later.
      if (is.na(oBank)) oBank <- row['OriginatorBank'] %>% str_split("\\s{3}") %>% .[[1]] %>% .[1] %>%
        gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all("^\\s+|\\s+$", "")
      #BeneficiaryBank is always empty, so set it to Credit Party.
      bBank <- row['CreditParty'] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
        str_replace_all("^\\s+|\\s+$", "")

      #There are two possibilities for the intermediary bank in this instance:
      # 1) Debit Party 2) None
      dpValue <- row['DebitParty']
      obValue <- row['OriginatorBank']
      if (grepl(dpValue, obValue, fixed=T)) {
        iBank <- NA
      } else {
        iBank <- row['DebitParty'] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
          str_replace_all("^\\s+|\\s$", "")
      }
      v <- c("Originator"=orig, "originatorAddress"=oAddr, "originatorAcctNum"=oAcctNum,
             "originatorBank"=oBank, "intermediateBank"=iBank, "beneficiaryBank"=bBank,
             "beneficiaryAcctNum"=bAcctNum, "beneficiaryAddress"=bAddr, "Beneficiary"=bnf)

    #Otherwise there are *Seqb variables, which means the Originator and/or the
    #Beneficiary fields are banks and need to be reset to the actual entities.
    } else {
      #Check if Originator == OriginatorSeqb. If not, then the Originator is (presumably)
      #a bank and should be reset to the actual entity.
      if (row['Originator'] == row['OriginatorSeqb']) {
        origField <- row['Originator'] %>% str_split("\\s{3,}|(?<=\\d{1,30}/(?=[A-Z]+)") %>% .[[1]]
        orig <- origField[2] %>% gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T)
        oAcctNum <- origField[1]
        if (length(origField) >= 3) {
          oAddr <- origField %>% .[3:length(.)] %>%
            str_replace_all("(?<=\\b\\w)\\s(?=\\w\\b)", "") %>%
            .[nchar(.)> 3] %>% paste(., collapse=" ")
        } else {
          oAddr <- NA
        }
        oBank <- row['OriginatorBank'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2] %>%
          gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
          str_replace_all("^\\s+|\\s+$", "")
        #There are four possibilities for the intermediary bank in this instance:
        # 1) Credit Party 2) Debit Party 3) Both or 4) Neither
        dpValue <- row['DebitParty']
        cpValue <- row['CreditParty']
        obValue <- row['OriginatorBank']
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
      } else {
        origField <- row['OriginatorSeqb'] %>% str_split("\\s{3,}|(?<=\\d{1,30}/(?=[A-Z]+)") %>% .[[1]]
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
        dpCheck <- row['DebitParty']
        origCheck <- row['Originator']
        if (!is.na(row['OriginatorBankSeqb']) & grepl(dpCheck, origCheck, ignore.case=T)) {
          oBank <- row['OriginatorBankSeqb']
        } else {
          oBank <- row['Originator'] %>% str_split("\\s{3,}") %>% .[[1]] %>% .[2] %>%
            gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
            str_replace_all("^\\s+|\\s+$", "")
        }
        iBank <- row['DebitParty'] %>%
          gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
          str_replace_all("^\\s+|\\s+$", "")
      }
      #Beneficiary Bank should be in the Beneficiary field and the Beneficiary should
      #be in the BeneficiarySeqb field.
      bnfField <- row['BeneficiarySeqb'] %>% str_split("\\s{3,}|(?<=\\d{1,30}/(?=[A-Z]+)") %>% .[[1]]
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

  #Clean up the data with common things seen across compliance
  dat$Originator %<>% str_replace_all("(?<=Llc).*", "") %>%
    gsub("\\b([A-z]{3})\\b", "\\U\\1", ., perl=T)

  return(dat)
}

parseJPMC <- function(file) {
  type = file_ext(file) %>% tolower()
  if (type == "xls") sheetNames <- .Call("read_xls_sheets", PACKAGE="readxl", file)
  if (type == "xlsx") sheetNames <- .Call("read_xlsx_sheets", PACKAGE="readxl", file)
  for (sheetName in sheetNames) {
    tmp <- read_excel(file, sheet=sheetName)
    names(tmp) %<>% str_replace_all("_", " ") %>%
      gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", ., perl=T) %>%
      str_replace_all(" ", "")
    date <- tmp$PaymentDate
    amount <- tmp$Amount %>% str_replace_all("[\\$,]", "")
    amount <- ifelse(grepl("\\.", amount), amount, paste(amount, "00", sep="."))
    cur <- ifelse(grepl("\\$", tmp$Amount), "USD", "Other")
    memo <- paste(tmp$DetPymt1, tmp$DetPymt2, tmp$DetPymt3, tmp$DetPymt4, collapse=" ")
    orig <- tmp$OrdCust2
    oAddr <- paste(tmp$OrdCust3, tmp$OrdCust4, collapse=" ")
    oAcctNum <- tmp$OrdCust1
    oBank <- tmp$DrAddr1
    bnf
  }
}

parseUBS <- function(file, password=NULL) {
  #placeholder
}

parseWireData <- function(file, bank, format, skip=0, password=NULL) {
  if (!file.exists(file)) stop("Invalid file path. Please be sure to use the full file path to a valid file.")
  if (tolower(bank) %in% c("boa", "bank of america")) {
    if (tolower(format) != "pdf") stop("Bank of America typically sends PDF files. Are you sure you sure this is the right format?")
    return(parseBOA(file))
  }
  if (tolower(bank) %in% c("bny mellon", "bnymellon")) {
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
  if (tolower(bank) == "hsbc") {
    if (!(tolower(format) %in% c("xls", "xlsx", "csv"))) stop("HSBC typically sends an Excel file. Are you sure this is the right format?")
    return(parseHSBC(file))
  }
  if (tolower(bank) %in% c("jpmc", "jpmorgan chase", "jp morgan chase", "jpmorganchase")) {
    if (!(tolower(format) %in% c("xls", "xlsx", "csv"))) stop("JPMC typically sends an Excel file. Are you sure this is the right format?")
    return(parseJPMC(file))
  }
  if (tolower(bank) %in% c("ubs", "ubs ag", "ubsag")) {
    if (!(tolower(format) %in% c("xls", "xlsx", "csv"))) stop("UBS typically sends an Excel file. Are you sure this is the right format?")
    return(parseUBS(file))
  }
}


