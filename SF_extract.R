library("gdata")
perl <- "C:/Strawberry/perl/bin/perl.exe"

setwd("C:/r_data") # Important to ensure everything is in correct path...
# Files needed:         Summary files (txt or csv format)
#                       Geography linking file (part of summary files)
#                       Geography file template (possible elimination in future)
#                       

## Variables to set

sf1 <- 'Virginia_All_Geographies_Not_Tracts_Block_Groups.zip'
sf2 <- 'Virginia_Tracts_Block_Groups_Only.zip'
sfapp <- 'ACS_2013_SF_5YR_Appendices.xls'
sftemplates <- '2013_5yr_Summary_FileTemplates.zip'
sfturl <- 'http://www2.census.gov/programs-surveys/acs/summary_file/2013/data/'
sfurl <- 'ftp://ftp.census.gov/acs2013_5yr/summaryfile/2009-2013_ACSSF_By_State_All_Tables/'
sfappurl <- 'ftp://ftp.census.gov/acs2013_5yr/summaryfile/'
geohead <- c('FILEID','STUSAB','SUMLEVEL','COMPONENT','LOGRECNO','US','REGION','DIVISION',
             'STATECE','STATE','COUNTY','COUSUB','PLACE','TRACT','BLKGRP','CONCIT','AIANHH',
             'AIANHHFP','AIHHTLI','AITSCE','AITS','ANRC','CBSA','CSA','METDIV','MACC','MEMI',
             'NECTA','CNECTA','NECTADIV','UA','BLANK','CDCURR','SLDU','SLDL','BLANK','BLANK',
             'ZCTA5','SUBMCD','SDELM','SDSEC','SDUNI','UR','PCI','BLANK','BLANK','PUMA5',
             'BLANK','GEOID','NAME','BTTR','BTBG','BLANK')
temp <- tempfile(fileext = 'xls')

        ## Note: 2013 5-year table templates do not include the geo template,
        ## but 3 and 1 year estimates do. Available here:
        ## http://www.census.gov/programs-surveys/acs/data/summary-file.html

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
        
        
        neededfiles <- list.files()
        
        
        # sets path according the appropriate summary level and checks for necessary files
        if (geo_level %in% c('150', '140')) {
                if (sf2 %in% neededfiles == FALSE) {
                        print("Downloading summary file. Please wait.")
                        download.file(paste0(sfurl, sf2), destfile = sf2, method = 'internal', mode = 'wb')
                        print ("Download complete.")
                }
                zloc <- sf2
        } else {
                if (sf1 %in% neededfiles == FALSE) {
                        print("Downloading summary file. Please wait.")
                        download.file(paste0(sfurl, sf1), destfile = sf1, method = 'internal', mode = 'wb')
                        print ("Download complete.")
                }
                zloc <- sf1
        }
        
        if (sfapp %in% neededfiles == FALSE) {
                print("Downloading required files. Please wait.")
                download.file(paste0(sfappurl, sfapp), destfile = sfapp, method = 'internal', mode = 'wb')
                print ("Download complete.")
        }
        
        if (sftemplates %in% neededfiles == FALSE) {
                print("Downloading required files. Please wait.")
                download.file(paste0(sfturl, sftemplates), destfile = sftemplates, method = 'internal', mode = 'wb')
                print ("Download complete.")
        }
        
        sumrecords <- sumlevelSet(geo_level)
        
        # creates a geography table to join with summary file table
        va_geo <- read.csv(unz(zloc, 'g20135va.csv', open = ''), header = FALSE,
                           col.names = geohead, colClasses = 'character')
        if (length(showConnections()) > 0) close(zloc)
        
        
        
        ## OLD CODE
        #va_geo <- read.csv(paste0(sf_location,'g20135va.csv'), header=FALSE,
         #                  col.names = geohead, colClasses = "character")
        
        # subsets geography table to desired summary level, adds simplified 'geoid2' column
        geo_sub <- subset(va_geo, SUMLEVEL == geo_level, select = sumrecords)
        geoid2 <- substr(geo_sub$GEOID, 8, nchar(geo_sub$GEOID))
        geo_sub <- cbind(geo_sub, geoid2, stringsAsFactors=FALSE)
        remove(geoid2, va_geo)
        #geo_sub
        
        #creates data frame for summary file based on identified table number
        acs_tables <- read.xls(sfapp, sheet = 1, stringsAsFactors = FALSE, colClasses = 'character',
                               perl = perl)
        #acs_tables$Table.Title[which(acs_tables$Table.Number == table_number)]
        
        # creates path variables depending on the table number and associated sequence file
        seqfilenumber <- acs_tables$Summary.File.Sequence.Number[which(acs_tables$Table.Number == table_number)]
        seqfilename <- paste0('e20135va', seqfilenumber, '000.txt')
        seqtemplatename <- paste0('Seq', as.integer(seqfilenumber),'.xls')

        seqcolsa <- strsplit(acs_tables$Summary.File.Starting.and.Ending.Positions[which(acs_tables$Table.Number == table_number)],
                             '-')
        seqcolsa <- seqcolsa[[1]]
        seqcolsb <- c(1:6, seqcolsa[[1]]:seqcolsa[[2]])
        
        # creates template...shell...word? must first unzip folder to get at xls files
        temp <- unzip(sftemplates, files = seqtemplatename)
        seqtable <- read.xls(temp, sheet = 1, perl = perl)
        
        classxxx <- vector(length = ncol(seqtable))
        classxxx[1:6] <- "character" ## Setting up initial header columns for logrecno join
        classxxx[7:length(classxxx)] <- "integer"
        
        # loading data into template
        s <- read.csv(unz(zloc, seqfilename, open = ''), header = FALSE, quote = '"',
                      col.names = colnames(seqtable), colClasses = classxxx)
        if (length(showConnections()) > 0) close(zloc)
        s <- s[seqcolsb]
        remove(seqtable, classxxx)
        
        
        s_geo <- merge(geo_sub, s, by = "LOGRECNO")
        
        if (write_table == TRUE) {
                zdir <- paste0(getwd(), '/OutputTables/')
                if (("OutputTables" %in% neededfiles) == FALSE) dir.create("OutputTables/")
                write.csv(s_geo, paste0('OutputTables/', table_number,'_', geo_level, '.csv'), row.names = FALSE)
                print(paste0('Table output to ... ', zdir, table_number,'_', geo_level, '.csv'))
        }
        
        return(s_geo)
}

# acs_tables$Table.Title[grep('Sex', acs_tables$Table.Title)]
## Keyword search