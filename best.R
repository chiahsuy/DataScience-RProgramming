best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    stdata<-outcomedata[which(outcomedata[,"State"]==state),]
    stdata[,odcln]<-as.numeric(stdata[,odcln])
    bsrate<-min(stdata[,odcln],na.rm=T)
    bshospitals<-sort(stdata[which(stdata[,odcln]==bsrate),"Hospital.Name"])
    bshospitals[1]    
}
