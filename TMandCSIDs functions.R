#This function is used to first fix trackman IDs so that they are correct to the player who's data is contained within the row.
#Then to add CollegeSplits IDs which are used in some of our internal apps. 
FixAddIDs <- function(newdata, existingdata){
  
  # Minor helper function for mode
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # First major helper function, fixing incorrect IDs
  fixIds <- function(newdata,existingdata){
    
    #Get our new batters who's IDs we need to fix
    batterlist <- unique(newdata$Batter)
    #Take their most common ID
    ids <- existingdata %>% filter(Year == "Spring_2025") %>% 
      group_by(Batter) %>% 
      summarise(BatterId = get_mode(BatterId)) %>% 
      filter(Batter %in% batterlist)
    
    #Get rid of existing batter IDs and fill with new ones
    newdata <- newdata %>% 
      select(-BatterId) %>% 
      left_join(ids, by = "Batter")
    
    #Get our new pitchers who's IDs we need to fix
    pitcherlist <- unique(newdata$Pitcher)
    #Take their most common ID
    ids <- existingdata %>% filter(Year == "Spring_2025") %>% 
      group_by(Pitcher) %>% 
      summarise(PitcherId = get_mode(PitcherId)) %>% 
      filter(Pitcher %in% pitcherlist)
    
    #Get rid of existing pitcher IDs and fill with new ones
    newdata <- newdata %>% 
      select(-PitcherId) %>% 
      left_join(ids, by = "Pitcher")
    
    #Get our new catchers who's IDs we need to fix
    catcherlist <- unique(newdata$Catcher)
    #Take their most common ID
    ids <- existingdata %>% filter(Year == "Spring_2025") %>% 
      group_by(Catcher) %>% 
      summarise(CatcherId = get_mode(CatcherId)) %>% 
      filter(Catcher %in% catcherlist)
    
    #Get rid of existing catcher IDs and fill with new ones
    newdata <- newdata %>% 
      select(-CatcherId) %>% 
      left_join(ids, by = "Catcher")
    
    #Send back the data with new IDs
    return(newdata)
  }
  
  AddCSIds <- function(df){
    #Get the translational data frame that relates TrackMan IDs to CollegeSplits IDs - Link removed for data privacy
    file_content = getURL()
    #Turn the file into a data frame
    tmIDS = read.csv(text = file_content, header = TRUE)
    
    #Batters
    #Rename columns for easier joining
    tmTemp <- tmIDS %>% rename(BatterId.CollegeSplits = cs_id, BatterId = trackman_id)
    #Add CollegeSplits IDs
    df <- df %>% 
      left_join(tmTemp, by = "BatterId")

    #Pitchers
    #Rename columns for easier joining
    tmTemp <- tmIDS %>% rename(PitcherId.CollegeSplits = cs_id, PitcherId = trackman_id)
    #Add CollegeSplits IDs
    df <- df %>% 
      left_join(tmTemp, by = "PitcherId")

    
    #Catchers
    #Rename columns for easier joining
    tmTemp <- tmIDS %>% rename(CatcherId.CollegeSplits = cs_id, CatcherId = trackman_id)
    #Add CollegeSplits IDs
    df <- df %>% 
      left_join(tmTemp, by = "CatcherId")
    
    #Send back the data frame with IDs added
    return(df)
  }
  
  #Run fix IDs
  newdata <- fixIds(newdata, existingdata)
  #Run add IDs
  newdata <- AddCSIds(newdata)
  #Return data with fixed TrackMan IDs and CollegeSplits IDs
  return(newdata)
}