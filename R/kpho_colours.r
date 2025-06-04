#' @title kpho_colours
#'
#' @description This function prints standard colour schemes for different geography types plotted on KPHO charts. 
#' 
#' @param geography String of the type of geography to get the colour scheme for. Options include: `"district"`, `"hcp"`, `"nhs_trust"`, `"pcn"`, `"utla_national"` and `"district_medway"`
#' @return A patchwork object.
#' 
#' @examples 
#' kpho_colours("district")
#' 
#' @export
kpho_colours <- function(geography){
    
    district <- c("Ashford"="", "Canterbury"="", "Dartford"="", "Dover"="", "Folkestone & Hythe"="", "Gravesham"="", "Maidstone"="", "Sevenoaks"="", "Swale"="", "Thanet"="",
        "Tonbridge & Malling"="", "Tunbridge Wells"="")

    hcp <- c("East Kent HCP"="#4d4dff", "West Kent HCP"="#ff4949", "Medway & Swale HCP"="#cfcf3b", "Dartford, Gravesham & Swanley HCP"="#3cb83c")

    nhs_trust <- c("Dartford and Gravesham NHS Trust"="", "East Kent Hospitals University NHS Foundation Trust"="", "Kent Community Health NHS Foundation Trust"="",
        "Kent and Medway NHS and Social Care Partnership Trust"="", "Maidstone and Tunbridge Wells NHS Trust"="", "Medway NHS Foundation Trust"="")

    utla_national <- c("Kent"="#2D7C82", "Medway"="#D04F30", "Kent & Medway"="#7D3F64", "South east region"="#338543", "England"="#0245A1")
    
    district_medway <- c("Ashford"="", "Canterbury"="", "Dartford"="", "Dover"="", "Folkestone & Hythe"="", "Gravesham"="", "Maidstone"="", "Sevenoaks"="", "Swale"="",
        "Thanet"="", "Tonbridge & Malling"="", "Tunbridge Wells"="", "Medway"="")
    
    if(geography=="district"){
        print(district)
    } else if(geography=="hcp"){
        print(hcp)
    } else if(geography=="nhs_trust"){
        print(nhs_trust)
    } else if (geography=="utla_national"){
        print(utla_national)
    } else if(geography=="district_medway"){
        print(district_medway)
    } else{
        stop("Please select a valid geography. You can choose from: district, hcp, nhs_trust or pcn.")
    }
}