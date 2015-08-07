library("xlsx")

setwd("C:/data/acs/ACS_2013_5year/") # Important to ensure everything is in correct path...
# Files needed:         Summary files (txt or csv format)
#                       Geography linking file (part of summary files)
#                       Geography file template (possible elimination in future)
#                       

## Variables to set

sumlevelSet <- function(summarylevel) {
        if (summarylevel == '040') {
                geolist <- c('LOGRECNO', 'STATE', 'GEOID', 'NAME')
                geolist
        } else if (summarylevel == '050') {
                geolist <- c('LOGRECNO', 'STATE', 'COUNTY', 'GEOID', 'NAME')
                geolist
        } else if (summarylevel == '140') {
                geolist <- c('LOGRECNO', 'STATE', 'COUNTY', 'TRACT', 'GEOID', 'NAME')
                geolist
        } else if (summarylevel == '150') {
                geolist <- c('LOGRECNO', 'STATE', 'COUNTY', 'TRACT', 'BLKGRP', 'GEOID', 'NAME')
                geolist
        } else print("Unable to process that summary level.")
}

SF_extract <- function(geo_level = '140', table_number = 'B03002', write_table = FALSE) {
        
        # sets path according the appropriate summary level
        if (geo_level %in% c('150', '140')) {
                sf_location <- "Virginia_Tracts_Block_Groups_Only/"
        } else {
                sf_location <- "Virginia_All_Geographies_Not_Tracts_Block_Groups/"
        }
        
        sumrecords <- sumlevelSet(geo_level)
        
        # creates a geography table to join with summary file table
        geo_head_desc <- read.csv("2013_SFGeoFileTemplate.csv", colClasses = "character")
        geocol <- colnames(geo_head_desc)
        va_geo <- read.csv(paste0(sf_location,'g20135va.csv'), header=FALSE, col.names = geocol, colClasses = "character")
        remove(geo_head_desc, geocol)
        
        # subsets geography table to desired summary level, adds simplified 'geoid2' column
        geo_sub <- subset(va_geo, SUMLEVEL == geo_level, select = sumrecords)
        geoid2 <- substr(geo_sub$GEOID, 8, nchar(geo_sub$GEOID))
        geo_sub <- cbind(geo_sub, geoid2, stringsAsFactors=FALSE)
        remove(geoid2, va_geo)
        #geo_sub
        
        #creates data frame for summary file based on identified table number
        acs_tables <- read.xlsx("ACS2013_5yr_App.xlsx", sheetIndex = 1, stringsAsFactors = FALSE)
        #acs_tables$Table.Title[which(acs_tables$Table.Number == table_number)]
        
        # creates path variables depending on the table number and associated sequence file
        seqfilenumber <- acs_tables$Summary.File.Sequence.Number[which(acs_tables$Table.Number == table_number)]
        seqfilename <- paste0('e20135va', seqfilenumber, '000.txt')
        seqtemplatename <- paste0('Seq', as.integer(seqfilenumber),'.xls')
        seqcolsa <- strsplit(acs_tables$Summary.File.Starting.and.Ending.Positions[which(acs_tables$Table.Number == table_number)],
                             '-')
        seqcolsa <- seqcolsa[[1]]
        seqcolsb <- c(1:6, seqcolsa[[1]]:seqcolsa[[2]])
        
        # creates template...shell...word?
        seqtable <- read.xlsx(paste0("2013_5yr_Summary_FileTemplates/",seqtemplatename), sheetIndex = 1)
        classxxx <- vector(length = ncol(seqtable))
        classxxx[1:6] <- "character" ## Setting up initial header columns for logrecno join
        classxxx[7:length(classxxx)] <- "integer"
        
        # loading data into template
        s <- read.csv(paste0(sf_location, seqfilename), header = FALSE, quote = '"',
                      col.names = colnames(seqtable), colClasses = classxxx)
        s <- s[seqcolsb]
        remove(seqtable, classxxx)
        
        
        s_geo <- merge(geo_sub, s, by = "LOGRECNO")
        
        if (write_table == TRUE) {
                contents <- list.files()
                zdir <- paste0(getwd(), '/OutputTables/')
                if (("OutputTables" %in% contents) == FALSE) dir.create("OutputTables/")
                write.csv(s_geo, paste0('OutputTables/', table_number,'_', geo_level, '.csv'), row.names = FALSE)
                print(paste0('Table output to ... ', zdir, table_number,'_', geo_level, '.csv'))
        }
        
        return(s_geo)
}

# acs_tables$Table.Title[grep('Sex', acs_tables$Table.Title)]
## Keyword search