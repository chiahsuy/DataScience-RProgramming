rankhospital <- function(state, outcome, num = "best") {
    library(stringi)
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    voutcome<-c("heart attack","heart failure","pneumonia")
    states<-unique(outcomedata$State)
    if(!state %in% states) stop("invalid state")
    if(!outcome %in% voutcome) {
        stop("invalid outcome")
    }else{
        head<-"Hospital.30.Day.Death..Mortality..Rates.from"
        #head<-"Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from"
        ocnm<-unlist(strsplit(stri_trans_totitle(outcome)," "))
        ocnm_ind<-ocnm[1]
        if (length(ocnm)>1){
            for (i in 2:length(ocnm)){
                ocnm_ind<-paste(ocnm_ind,ocnm[i],sep=".")
            }
        }
        odcln<-paste(head,ocnm_ind,sep=".")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    stdata<-outcomedata[which(outcomedata[,"State"]==state),]
    stdata[,odcln]<-as.numeric(stdata[,odcln])
    stdata<-stdata[order(stdata[,"Hospital.Name"]),]
    rkocdata<-stdata[order(stdata[,odcln],na.last=NA),]
    rkhospitals<-rkocdata[,"Hospital.Name"]

    if(num=="best"){
        return(rkhospitals[1])
    }else if(num=='worst'){
        return(rkhospitals[length(rkhospitals)])
    }else{
        ind<-as.numeric(num)
         if (ind>length(rkhospitals)){
            return(NA)
        }else{
            return(rkhospitals[ind])
        }
    }
}
