PitchTypeDisplay <- function(newdf, refdf){
  #Destroy Sweepers
  refdf$TaggedPitchType <- ifelse(refdf$TaggedPitchType == "Sweeper", "SL", refdf$TaggedPitchType)
  newdf$TaggedPitchType <- ifelse(newdf$TaggedPitchType == "Sweeper", "SL", newdf$TaggedPitchType)
  
  #Prepare medianbreaks aggregated df. Bang NAs, create HB, IVB, and Velo, join, and bang undefineds. 
  refdf <- refdf %>% filter(!is.na(TaggedPitchType))
  hb <- refdf %>% filter(!is.na(HorzBreak)) %>% group_by(Pitcher,TaggedPitchType) %>% summarize(HorzBreak = median(HorzBreak), n = n())
  #Second agg df to help create percents
  toBind <- hb %>% group_by(Pitcher) %>% summarise(nT = sum(n))
  #Join the two and create percents
  hb <- left_join(hb, toBind)
  hb$PCT <- round(hb$n/hb$nT,2)
  hb <- hb %>% select(-n,-nT)
  #Create IVB and velo agg dfs
  ivb <- refdf %>% filter(!is.na(InducedVertBreak)) %>% group_by(Pitcher, TaggedPitchType) %>% summarize(InducedVertBreak = median(InducedVertBreak))
  velo <- refdf %>% filter(!is.na(RelSpeed)) %>% group_by(Pitcher, TaggedPitchType) %>% summarize(RelSpeed = median(RelSpeed))
  #Join the three - use inner to rid all NAs
  agg <- inner_join(hb,ivb, by = c("Pitcher","TaggedPitchType"))
  agg <- inner_join(agg, velo, by = c("Pitcher","TaggedPitchType"))
  #Some filters for quality
  agg <- agg %>% filter(TaggedPitchType != "Undefined" & PCT > 0.03) 
  
  #Prepare df to be displayed - start with TaggedPitchType so that we can leave it as default w/o ball flight data
  newdf$PitchTypeDisplay <- newdf$TaggedPitchType
  
  #Initialize list of rows
  chunk_list <- list()
  
  #Pitcher loop
  for(m in unique(newdf$Pitcher)){
    #Filter dfs per pitcher
    miniagg <- agg %>% filter(Pitcher == m)
    data <- newdf %>% filter(Pitcher == m)
    new_row <- NA
    
    #Override if Slider and Cutter are likely one pitch tagged as two -> make one pitch
    if(all(c("SL","CT") %in% miniagg$TaggedPitchType)) {
      r <- crossdist(miniagg$HorzBreak[miniagg$TaggedPitchType == "SL"],miniagg$InducedVertBreak[miniagg$TaggedPitchType == "SL"],
                     miniagg$HorzBreak[miniagg$TaggedPitchType == "CT"],miniagg$InducedVertBreak[miniagg$TaggedPitchType == "CT"])
      if(r < 3){
        cp <- miniagg$PCT[miniagg$TaggedPitchType == "CT"]
        sp <- miniagg$PCT[miniagg$TaggedPitchType == "SL"]
        new_row <- data.frame(Pitcher = miniagg$Pitcher[1],
                              TaggedPitchType = NA,
                              HorzBreak = as.double((sp*miniagg$HorzBreak[miniagg$TaggedPitchType == "SL"] + cp*miniagg$HorzBreak[miniagg$TaggedPitchType == "CT"])/(cp+sp)),
                              PCT = cp+sp,
                              InducedVertBreak = as.double((sp*miniagg$InducedVertBreak[miniagg$TaggedPitchType == "SL"] + cp*miniagg$InducedVertBreak[miniagg$TaggedPitchType == "CT"])/(cp+sp)),
                              RelSpeed = as.double((sp*miniagg$RelSpeed[miniagg$TaggedPitchType == "SL"] + cp*miniagg$RelSpeed[miniagg$TaggedPitchType == "CT"])/(cp+sp))
        )
        new_row$TaggedPitchType <- ifelse((new_row$InducedVertBreak > 0 & 1.5*new_row$InducedVertBreak > abs(new_row$HorzBreak)),"CT","SL")
        miniagg <- miniagg %>% filter(!(TaggedPitchType %in% c("CT","SL")))
        miniagg <- rbind(miniagg, new_row)
      }
    }
    
    #Override if Slider and Curve are likely one pitch tagged as two -> make one pitch
    if(all(c("SL","CB") %in% miniagg$TaggedPitchType)) {
      r <- crossdist(miniagg$HorzBreak[miniagg$TaggedPitchType == "SL"],miniagg$InducedVertBreak[miniagg$TaggedPitchType == "SL"],
                     miniagg$HorzBreak[miniagg$TaggedPitchType == "CB"],miniagg$InducedVertBreak[miniagg$TaggedPitchType == "CB"])
      if(r < 3){
        cp <- miniagg$PCT[miniagg$TaggedPitchType == "CB"]
        sp <- miniagg$PCT[miniagg$TaggedPitchType == "SL"]
        new_row <- data.frame(Pitcher = miniagg$Pitcher[1],
                              TaggedPitchType = NA,
                              HorzBreak = as.double((sp*miniagg$HorzBreak[miniagg$TaggedPitchType == "SL"] + cp*miniagg$HorzBreak[miniagg$TaggedPitchType == "CB"])/(cp+sp)),
                              PCT = cp+sp,
                              InducedVertBreak = as.double((sp*miniagg$InducedVertBreak[miniagg$TaggedPitchType == "SL"] + cp*miniagg$InducedVertBreak[miniagg$TaggedPitchType == "CB"])/(cp+sp)),
                              RelSpeed = as.double((sp*miniagg$RelSpeed[miniagg$TaggedPitchType == "SL"] + cp*miniagg$RelSpeed[miniagg$TaggedPitchType == "CB"])/(cp+sp))
        )
        new_row$TaggedPitchType <- ifelse(((abs(new_row$InducedVertBreak)) > abs(new_row$HorzBreak) & abs(new_row$InducedVertBreak) > 7.5),"CB","SL")
        miniagg <- miniagg %>% filter(!(TaggedPitchType %in% c("CB","SL")))
        miniagg <- rbind(miniagg, new_row)
      }
    }
    #Case for wake pitchers or for no ball flight data - leave TaggedPitchType as is
    if(nrow(data) > 0 & nrow(miniagg) > 0 & (is.na(data$PitcherTeam[1]) | data$PitcherTeam[1] != "WAK_DEA")){
      for(l in 1:nrow(data)){
        #For pitches we don't have ball flight data for - revert to TaggedPitchType
        if(is.na(data$HorzBreak[l]) | is.na(data$InducedVertBreak[l])){
          data$PitchTypeDisplay[l] <- data$TaggedPitchType[l]
        } else {
          #For pitches we do - euclidean distance of each pitch with each median break
          threshold <- 10000
          for(n in 1:nrow(miniagg)){
            comp <- crossdist(data$HorzBreak[l], data$InducedVertBreak[l], 
                              miniagg$HorzBreak[n], miniagg$InducedVertBreak[n])
            if(comp < threshold) {
              data$PitchTypeDisplay[l] <- miniagg$TaggedPitchType[n]
              threshold <- comp
            }
          }
        }
      }
      #Do this in the interim so that velo retags don't break due to weird data types
      data$PitchTypeDisplay <- as.character(data$PitchTypeDisplay)
      
      #Begin velocity retags
      #Holder column for bringing FBs back to FFs and FTs
      data$holder <- data$PitchTypeDisplay
      #Filter the agg df to just FBs and CHs
      maFBCH <- miniagg %>% filter(TaggedPitchType %in% c("CH","FF","FT"))
      #Enure we have types we need
      if (all("CH" %in% maFBCH$TaggedPitchType) & nrow(maFBCH) == 2) {
        #If only two types, convert to FB and CH
        maFBCH$TaggedPitchType[2] <- "FB"
      } else if (nrow(maFBCH) == 3) {
        #If all three, take weighted averages for FB
        maFBCH[4,] <- NA
        maFBCH$Pitcher[4] <- maFBCH$Pitcher[1]
        maFBCH$TaggedPitchType[4] <- "FB"
        maFBCH$PCT[4] <- sum(maFBCH$PCT[2] + maFBCH$PCT[3])
        maFBCH$RelSpeed[4] <- sum((maFBCH$PCT[2]/maFBCH$PCT[4])*maFBCH$RelSpeed[2] + (maFBCH$PCT[3]/maFBCH$PCT[4])*maFBCH$RelSpeed[3])
        maFBCH <- maFBCH %>% filter(TaggedPitchType %in% c("CH","FB"))
      }
      
      #Check a few conditions before running velo retags - CH and one FB exist and are above 5%
      if(nrow(maFBCH) > 1 & "CH" %in% maFBCH$TaggedPitchType){
        if((nrow(maFBCH) == 2 & maFBCH$PCT[1] > 0.05 & maFBCH$PCT[2] > 0.05)) {
          #Change data to FB and CH only
          data$PitchTypeDisplay <- ifelse(data$PitchTypeDisplay %in% c("FF","FT"),"FB",data$PitchTypeDisplay)
          for(l in 1:nrow(data)) {
            if(data$PitchTypeDisplay[l] %in% c("CH","FB")) {
              #For pitches we don't have ball flight data for - revert to TaggedPitchType
              if(is.na(data$RelSpeed[l])) {
                data$PitchTypeDisplay[l] <- data$TaggedPitchType[l]
              } else {
                #For pitches we do - distance of each pitch with each median speed
                threshold <- 10000
                for(n in 1:nrow(maFBCH)){
                  comp <- abs(data$RelSpeed[l] - maFBCH$RelSpeed[n])
                  if(comp < threshold) {
                    data$PitchTypeDisplay[l] <- maFBCH$TaggedPitchType[n]
                    threshold <- comp
                  }
                }
                #Now to convert our FBs back to FF and FT
                if(data$PitchTypeDisplay[l] == "FB"){
                  #If we originally chose 1, go back to it
                  if(data$holder[l] %in% c("FF","FT")){
                    data$PitchTypeDisplay[l] <- data$holder[l]
                    #If pitcher throws both, euclidean distance again
                  } else if (all(c("FF", "FT") %in% miniagg$TaggedPitchType)) {
                    if (!is.na(data$HorzBreak[l]) & !is.na(data$InducedVertBreak[l])){
                      threshold <- 10000
                      for(n in c("FF","FT")) {
                        h <- miniagg %>% filter(TaggedPitchType == n)
                        comp <- crossdist(data$HorzBreak[l], data$InducedVertBreak[l],
                                          h$HorzBreak[1], h$InducedVertBreak[1])
                        if(comp < threshold) {
                          data$PitchTypeDisplay[l] <- n
                          threshold <- comp
                        }
                      }
                    } else {
                      data$PitchTypeDisplay[l] <- data$TaggedPitchType[l]
                    }
                    #If only one, use that one
                  } else if ("FF" %in% miniagg$TaggedPitchType & !("FT" %in% miniagg$TaggedPitchType)) {
                    data$PitchTypeDisplay[l] <- "FF"
                  } else if ("FT" %in% miniagg$TaggedPitchType & !("FF" %in% miniagg$TaggedPitchType)) {
                    data$PitchTypeDisplay[l] <- "FT"
                  } else {
                    data$PitchTypeDisplay[l] <- data$TaggedPitchType[l]
                  }
                }
              }
            }
          }
          #Get rid of holder
          #if("holder" %in% names(data)){
          #data <- data %>% select(-holder)
          #}
        }
      }
    }
    #Fix data types again to prevent anything weird
    data$PitchTypeDisplay <- as.character(data$PitchTypeDisplay)
    #Add this pitchers rows to the list
    chunk_list[[m]] <- data
  }
  #After pitcher loop bind all chunks to the df
  newdf <- bind_rows(chunk_list)
  #Return and end
  return(newdf)
}