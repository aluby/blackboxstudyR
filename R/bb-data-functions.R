getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

score_bb_data = function(fbi_bb_data, scoring_method){
  # Check dims & variables of fbi_bb_data
  scored_vector = rep(NA, nrow(fbi_bb_data))
  if(scoring_method == "inconclusive_mcar"){
    scored_vector = as.integer((fbi_bb_data$Compare_Value == 'Exclusion' & fbi_bb_data$Mating == 'Non-mates') | 
      (fbi_bb_data$Compare_Value == 'Individualization' & fbi_bb_data$Mating == 'Mates'))
    scored_vector[fbi_bb_data$Compare_Value == 'Inconclusive'] = NA
  }
  else
  if(scoring_method == "inconclusive_incorrect"){
      scored_vector = as.integer((fbi_bb_data$Compare_Value == 'Exclusion' & fbi_bb_data$Mating == 'Non-mates') | 
        (fbi_bb_data$Compare_Value == 'Individualization' & fbi_bb_data$Mating == 'Mates'))
      scored_vector[fbi_bb_data$Compare_Value == 'Inconclusive'] = 0
  }
  else
  if(scoring_method == "partial_credit"){
    scored_vector = 2*as.integer((fbi_bb_data$Compare_Value == 'Exclusion' & fbi_bb_data$Mating == 'Non-mates') | 
      (fbi_bb_data$Compare_Value == 'Individualization' & fbi_bb_data$Mating == 'Mates'))
    scored_vector[fbi_bb_data$Compare_Value == 'Inconclusive'] = 1
  }
  else{
    consensus_reason = rep(NA, nrow(fbi_bb_data))
  for(resp in 1:nrow(fbi_bb_data)){
    pair_id = fbi_bb_data$Pair_ID[resp]
    # Pull reasons for inconclusives for that pair ID EXCLUDING current response
    consensus_reason[resp] = getmode(fbi_bb_data$Inconclusive_Reason[-resp][fbi_bb_data$Pair_ID[-resp] == pair_id])
  }
    if(scoring_method == "no_consensus_mcar"){
      scored_vector = as.integer((fbi_bb_data$Compare_Value == 'Exclusion' & fbi_bb_data$Mating == 'Non-mates') | 
                                   (fbi_bb_data$Compare_Value == 'Individualization' & fbi_bb_data$Mating == 'Mates') |
                                   (fbi_bb_data$Compare_Value == "Inconclusive" & as.integer(fbi_bb_data$Inconclusive_Reason) == consensus_reason))
    }
    else
      if(scoring_method == "no_consensus_incorrect"){
        scored_vector = as.integer((fbi_bb_data$Compare_Value == 'Exclusion' & fbi_bb_data$Mating == 'Non-mates') | 
                                     (fbi_bb_data$Compare_Value == 'Individualization' & fbi_bb_data$Mating == 'Mates') |
                                     (fbi_bb_data$Compare_Value == "Inconclusive" & as.integer(fbi_bb_data$Inconclusive_Reason) == consensus_reason))
        scored_vector[fbi_bb_data$Compare_Value=='Inconclusive' & is.na(consensus_reason)] = 0
      }
    
  }
  return(scored_vector)
}

## Needs check on data
irt_data_bb = function(fbi_bb_data, scored_responses){
  if(any(!"Examiner_ID" %in% names(fbi_bb_data), !"Pair_ID" %in% names(fbi_bb_data))){
    stop("FBI Black Box Data required - Examiner_ID or Pair_ID not found.")
  }
  if(any(scored_responses < 0, na.rm = TRUE)){
    stop("scored_responses must be non-negative.")
  }
  if(nrow(fbi_bb_data)!=length(scored_responses)){
    stop("scored_responses incorrect length - must match data.")
  }
  exID = as.integer(as.factor(fbi_bb_data$Examiner_ID))[which(!is.na(scored_responses))]
  qID = as.integer(as.factor(fbi_bb_data$Pair_ID))[which(!is.na(scored_responses))]
  return(list(
    N = length(which(!is.na(scored_responses))),
    I = length(unique(qID)),
    J = length(unique(exID)),
    y = scored_responses[which(!is.na(scored_responses))],
    ii = qID,
    jj = exID
  ))
}

## Needs check on parameters
fit_irt = function(irt_data, model = "rasch", iterations = 600, n_chains = 4){
  if(model == "rasch"){
    model = stan("rasch.stan", data = irt_data, iter = iterations, chains = n_chains)
  }
  else
    if(model == "2pl"){
      model = stan("2pl.stan", data = irt_data, iter = iterations, chains = n_chains)
    }
  else
    if(model == "pcm"){
      model = stan("pcm.stan", data = irt_data, iter = iterations, chains = n_chains)
    }
  else{
    cat("Model not recognized: try \"rasch\", \"2pl\" or \"pcm\"")
    return(NULL)
  }
  return(model)
}
