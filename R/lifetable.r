#' @title lt
#' 
#' @description This function creates a lifetable of a given dataframe of deaths by single year of age. It does not accurately calculate life expectancy for those below age 1. Please see the ONS guidance for how to caluclate quarterly life expectancy for those below age 1.
#' 
#' @param df A dataframe with two columns: single year of age and the associated deaths within a certain time period. Note: all ages, even if there are 0 deaths recorded, must be present for an accurate calculation of life expectancy.
#' 
#' @return A dataframe with all constituent elements of a lifetable calculation.
#' 
#' @export 
lt <- function(df){

    if(!is.numeric(df[,1])){
        df[,1] <- as.numeric(df[,1])
    } else{} #ensures that the age variable is numeric

    if(!is.numeric(df[,2])){
        df[,2] <- as.numeric(df[,2])
    } else{} #ensures that the deaths variable is numeric

    if(ncol(df)!=2){
        stop("The dataframe supplied has more than two variables. Please ensure the dataframe only has one variable for ages and another for deaths corresponding to that ages.")
    } else{} 

    warning("This function requires a dataframe with consistent age groupings. The size of each age group will change the interval used to calculate Lx.")
    warning("This function does not currently produce accurate life expectancy calculations at age 0. Please use alternative formulae if you require this.")

    df <- df %>% mutate(
        mx=df$d/df$pop,
        qx=2*mx/(2+mx)  
    ) #creates age specific death rate, probability of dying, and probability of survival

    df$qx[nrow(df)] <- 1 #the final category assumes 100% probability of death

    df <- df %>% mutate(
        Px=1-qx
    ) #probability of survival is 1-probability of death - so for the last age cateogry the probability of survival is 0%

    df$lx <- NA #creates number alive at age x column
    df$lx[1] <- 100000 #set age 0 to 100,000 standard population

    for(i in 2:nrow(df)){
        df$lx[i] <- df$lx[i-1] * (1-df$qx[i-1])
    } #for loop which runs from row 2 to the end and creates the number of people alive at each age based on the proceeding probability of survival at age x

    for(i in 1:(nrow(df)-1)){
        df$dx[i] <- df$lx[i]-df$lx[i+1]
    } #for loop which runs from row 1 to the second last row to create the number of people died in the interval (i.e. number of deaths at age i minus the deaths at age i + 1)

    df$dx[nrow(df)] <- df$lx[nrow(df)] #assumed that in the final age category interval everyone dies

    for(i in 1:(nrow(df)-1)){
        df$Lx[i] <- (df$lx[i] + df$lx[i+1])/2
    } #for loop to create person-years lived in the interval

    df$Lx[nrow(df)] <- df$lx[nrow(df)]/df$mx[nrow(df)] #Note: See the warning below. This is a simplified assumption of deaths spread evenly across the final interval which is assumed to run to 105 years.

    warning("The value for the years of life lived in the final row of this table is only an estimate using a simplified assumption.")

    df$Tx <- NA #initalises the column

    df$Tx[nrow(df)] <- df$Lx[nrow(df)] #sets all the Lx values to be Tx as we need the final value for reverse cummulative logic

    for(i in (nrow(df)-1):1){
        df$Tx[i] <- df$Lx[i] + df$Tx[i+1]
    } #reverse cumulative for loop to create the number of person years lived from age x to the oldest age

    df <- df %>% mutate(ex=Tx/lx)

    return(df) #returns the new dataframe once complete

}