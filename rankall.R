rankall <- function(outcome, num = "best") {
    library(stringi)
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    voutcome<-c("heart attack","heart failure","pneumonia")
    states<-unique(outcomedata$State)
    
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
    ocdata<-outcomedata[,c("Hospital.Name","State",odcln)]
    ocdata[,odcln]<-as.numeric(ocdata[,odcln])
    stocdata<-split(ocdata,ocdata$State)
    stocdata<-lapply(stocdata, function(x) x[order(x[,"Hospital.Name"]),])
    stocdata<-lapply(stocdata, function(x) x[order(x[,3],na.last=NA),])
    
    rank.st.hp<-function(x,ind){
        col<-c("Hospital.Name","State")
        if(ind=="best"){
            return (x[1,col])
        }else if(num=='worst'){
            return (x[dim(x)[1],col])
        }else{
            indx<-as.numeric(ind)
            if (indx>dim(x)[1]){
                return(c(NA,x[1,col[2]]))
            }else{
                return(x[indx,col])
            }
        }
    }
    
    rkhospital<-sapply(stocdata, rank.st.hp,ind=num)
    rk.all<-as.data.frame(t(rkhospital))
    colnames(rk.all)<-c("hospital","state")
    rk.all
    
    
}
