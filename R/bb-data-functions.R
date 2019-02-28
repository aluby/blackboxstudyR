library(rstan)

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
      scored_vector[fbi_bb_data$Compare_Value=='Inconclusive' & is.na(consensus_reason)] = NA
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
  exID = as.integer(fbi_bb_data$Examiner_ID[which(!is.na(scored_responses))])
  qID = as.integer(fbi_bb_data$Pair_ID[which(!is.na(scored_responses))])
  return(list(
    N = length(which(!is.na(scored_responses))),
    I = max(qID),
    J = max(exID),
    y = scored_responses[which(!is.na(scored_responses))],
    ii = qID,
    jj = exID
  ))
}

## Needs check on parameters
fit_irt = function(irt_data, model = "rasch", iterations = 600, n_chains = 4){
  if(!(model %in% c("rasch", "2pl", "pcm"))){
    stop("Model not recognized: try \"rasch\", \"2pl\" or \"pcm\"")
  }
  if(!any(c("y", "ii", "jj", "N", "J", "I") %in% names(irt_data))){
    stop("Data does not contain necessary elements, try irt_data_bb(..)")
  }
  if(model == "rasch"){
    model = stan("../src/stan-files/rasch.stan", data = irt_data, iter = iterations, chains = n_chains)
  }
  else
    if(model == "2pl"){
      model = stan("../src/stan-files/2pl.stan", data = irt_data, iter = iterations, chains = n_chains)
    }
  else
    if(model == "pcm"){
      model = stan("../src/stan-files/pcm.stan", data = irt_data, iter = iterations, chains = n_chains)
    }
  return(model)
}


bb_person_score = function(fbi_bb_data, scored_responses){
  if(nrow(fbi_bb_data)!=length(scored_responses)){
    stop("scored_responses incorrect length - must match data.")
  }
  fbi_exID = as.integer(fbi_bb_data$Examiner_ID)
  exID = unique(as.integer(fbi_bb_data$Examiner_ID))
  res = data.frame(exID, score = rep(NA, length(exID)))
  for(ex in 1:length(unique(exID))){
    res[which(exID == ex),2] = mean(scored_responses[which(fbi_exID == ex)],
                                    na.rm = TRUE)
  }
  if(max(scored_responses, na.rm = TRUE)>1){
    res$score = res$score/max(scored_responses, na.rm = TRUE)
  }
  return(res)
}

bb_item_score = function(fbi_bb_data, scored_responses){
  if(nrow(fbi_bb_data)!=length(scored_responses)){
    stop("scored_responses incorrect length - must match data.")
  }
  fbi_qID = as.integer(fbi_bb_data$Pair_ID)
  qID = unique(fbi_qID)
  res = data.frame(qID,
                   score = rep(NA, length(qID)),
                   pct_na = rep(NA, length(qID)),
                   n_ans = rep(NA, length(qID)))
  for(qq in 1:length(unique(qID))){
    res$score[which(qID == qq)] = mean(scored_responses[which(fbi_qID == qq)],
                                    na.rm = TRUE)
    res$pct_na[which(qID == qq)] = mean(is.na(scored_responses[which(fbi_qID == qq)]))
    res$n_ans[which(qID == qq)] = length(scored_responses[which(fbi_qID == qq)])
    }
  if(max(scored_responses, na.rm = TRUE)>1){
    res$score = res$score/max(scored_responses, na.rm = TRUE)
  }
  return(res)
}
