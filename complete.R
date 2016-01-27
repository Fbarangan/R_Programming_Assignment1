### Assigment 1 for  R Programming
### Felix Barangan, MS, RN, MLS(ASCP)
### Certified Oracle Implementation Specialist
### Nortehrn Kentucky University
##############################################################################
# Start of Function listcomplete
#
listcomplete <- function (directory, id = 1:332){
        # create a list with the users input
        # concept and ideas was based on https://github.com/derekfranks/practice_assignment


        list_specdata <- list.files(directory,full.names = TRUE)

        # Pass the id input to a place holder PM_ID which is used to determine the csv file to read.
        # This selected csv will then be saved to  ID_file_specdata
        PM_ID <- id
        ID_file_specdata <- list_specdata[PM_ID]

        # Create complete_case to be used as an empty data frame. then start loop
        complete_cases  <- data.frame()
        for (i in seq_along(PM_ID)) {

                read_csv_file <- read.csv(ID_file_specdata[i])
                read_csv_file_complete <- sum(complete.cases(read_csv_file))

                # Combine read csv into the empty data frame "complete_case"
                # the next csv selected is added as a next row, thereby preventing it from showing repeated column
                # and title.
                complete_cases <- rbind(complete_cases,
                                        complete_cases <- data.frame(ID = PM_ID[i], nobs = read_csv_file_complete)
                )
        }
        PM_ID  <<- PM_ID
        directory <<- directory
        print(complete_cases)
}

#  - End of Function LIstcomplete -
###########################################

## -Start of corr function -

corr <- function(directory, threshold = 0){

        PM_Threshold <- subset(listcomplete(directory,1:332)$ID, listcomplete(directory, 1:332)$nobs > threshold)
        # extract the data from the selected PM
        list_specdata  <- list.files(directory, full.names = TRUE) # i need this to get the csv.

        #Star an empty vector to past correlation results.
        cor_data <- c()

        for (i in PM_Threshold ){

                read_csv_file <- na.omit(read.csv(list_specdata[i])) # clean data. without null. i will get the

                # Correlate Sulfate and nitrate
                read_csv_file_cor <- cor(read_csv_file$sulfate, read_csv_file$nitrate)

                # will combine read_csv_file_cor using the empty cor_data vertex
                cor_data   <- c(cor_data, read_csv_file_cor)

        }
        print(cor_data)

}


