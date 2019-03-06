#' Results from FBI "black box" study
#'
#' @format A data frame with 17,121 rows and 8 variables
#' \describe{
#'   \item{Examiner_ID}{}
#'   \item{Pair_ID}{}
#'   \item{Mating}{}
#'   \item{Latent_Value}{}
#'   \item{Compare_Value}{}
#'   \item{Inconclusive_Reason}{If Compare_Value is Inconclusive, what is the reason given?}
#'   \item{Exclusion_Reason}{If Compare_Value is Exclusion, what is the reason given?}
#'   \item{Difficulty}{}
#' }
#' @source \url{https://www.fbi.gov/services/laboratory/scientific-analysis/counterterrorism-forensic-science-research/black-box-study-results}
"TestResponses"


#' Calculate the mode of a vector
#'
#' @param v vector to find mode of
#' @return The mode of \code{v}
#' @examples
#' getmode(c(1,1,1,0,0))
#' @export
getmode <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

#' Score the FBI black box data
#'
#' @param fbi_bb_data Dataset of the form of TestResponses
#' @param scoring_method Which scoring method to use: inconclusive_mcar, inconclusive_incorrect, partial_credit, no_consensus_mcar, no_consensus_incorrect
#' @return A vector of scored responses (1 row in fbi_bb_data = 1 entry)
#' @examples
#' score_bb_data(TestResponses, "inconclusive_mcar")
#' @export
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

#' Format the black box data to fit the IRT model
#'
#' @param fbi_bb_data Dataset of the form of TestResponses
#' @param scored_responses Scored responses (result of score_bb_data())
#' @return A list in the format needed for fit_irt()
#' @examples
#' \dontrun{
#' irt_data_bb(TestResponses, im_scored)
#' }
#' @export
irt_data_bb = function(fbi_bb_data, scored_responses){
  ## Needs check on data
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

#' Fit an IRT model
#'
#' @param irt_data List formatted as in irt_data_bb()
#' @param model Type of IRT model to fit (rasch, 2pl, pcm)
#' @param iterations Number of MCMC iterations per chain
#' @param n_chains Number of MCMC chains to run
#' @return stan object
#'
#' @importFrom rstan stan
#'
#' @examples
#' \dontrun{
#' irt_data_bb(TestResponses, im_scored)
#' }
#' @export
fit_irt = function(irt_data, model = "rasch", iterations = 600, n_chains = 4){
  if(!(model %in% c("rasch", "2pl", "pcm"))){
    stop("Model not recognized: try \"rasch\", \"2pl\" or \"pcm\"")
  }
  if(!any(c("y", "ii", "jj", "N", "J", "I") %in% names(irt_data))){
    stop("Data does not contain necessary elements, try irt_data_bb(..)")
  }
  if(model == "rasch"){
    model = stan(system.file("src/stan-files/rasch.stan", package = "blackboxstudyR"), data = irt_data, iter = iterations, chains = n_chains)
  }
  else
    if(model == "2pl"){
      model = stan(system.file("src/stan-files/2pl.stan", package = "blackboxstudyR"), data = irt_data, iter = iterations, chains = n_chains)
    }
  else
    if(model == "pcm"){
      model = stan(system.file("src/stan-files/pcm.stan", package = "blackboxstudyR"), data = irt_data, iter = iterations, chains = n_chains)
    }
  return(model)
}

#' Calculate observed score for each participant
#'
#' @param fbi_bb_data Dataset of the form of TestResponses
#' @param scored_responses Scored responses (result of score_bb_data())
#' @return Data frame of scores, one row for each participant
#' @examples
#' \dontrun{
#' bb_person_score(TestResponses, im_scored)
#' }
#' @export
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

#' Calculate observed score for each item
#'
#' @param fbi_bb_data Dataset of the form of TestResponses
#' @param scored_responses Scored responses (result of score_bb_data())
#' @return Data frame of scores, one row for each item
#' @examples
#' \dontrun{
#' bb_item_score(TestResponses, im_scored)
#' }
#' @export
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

#' Calculate error rates and avg question difficulty for each participant
#'
#' @param fbi_bb_data Dataset of the form of TestResponses
#' @param q_diff Question difficulty (from IRT analysis)
#' @return Data frame of avg difficulty, percent of items skipped, FPR, FNR, one row for each participant
#' @examples
#' \dontrun{
#' error_rate_analysis(TestResponses, question_difficulties)
#' }
#' @export
error_rate_analysis = function(fbi_bb_data, q_diff){
  qID = as.numeric(fbi_bb_data$Pair_ID)
  exID = as.numeric(fbi_bb_data$Examiner_ID)
  q_diff = q_diff[qID]
  is.skipped = fbi_bb_data$Latent_Value == 'NV' | fbi_bb_data$Compare_Value == 'Inconclusive'
  false.pos = fbi_bb_data$Compare_Value == 'Individualization' & fbi_bb_data$Mating == 'Non-mates'
  false.neg = fbi_bb_data$Compare_Value == 'Exclusion' & fbi_bb_data$Mating == 'Mates'
  ex_resp_data = data.frame(exID = 1:169,
                            avg_diff = rep(NA, 169),
                            pct_skipped = rep(NA, 169),
                            fpr = rep(NA, 169),
                            fnr = rep(NA, 169))
  for(ex in unique(exID)){
    ex_resp_data$avg_diff[ex] = mean(q_diff[which(exID == ex)], na.rm = TRUE)
    ex_resp_data$pct_skipped[ex] = sum(is.skipped[which(exID == ex)]) / sum(exID == ex)
    ex_resp_data$fpr[ex] = sum(false.pos[which(exID == ex)], na.rm = TRUE) /
      sum(!is.skipped[which(exID == ex)])
    ex_resp_data$fnr[ex] = sum(false.neg[which(exID == ex)], na.rm = TRUE) /
      sum(!is.skipped[which(exID == ex)])
  }
  return(ex_resp_data)
}

#' Calculate MCMC intervals for participant estimates
#'
#' @param stan_model_output Stan object from an IRT analysis
#' @return Tibble of 50% and 95% posterior intervals for each theta estimate
#'
#' @importFrom bayesplot mcmc_intervals_data
#'
#' @examples
#' person_mcmc_intervals(im_model)
#' @export
person_mcmc_intervals = function(stan_model_output){
  intervals = mcmc_intervals_data(as.array(stan_model_output),
                      regex_pars = 'theta',
                      prob_outer = .95) %>%
    mutate(., exID = as.integer(substr(parameter, 7, nchar(as.character(parameter)) - 1)))
  return(intervals)
}

#' Calculate MCMC intervals for item estimates
#'
#' @param stan_model_output Stan object from an IRT analysis
#' @return Tibble of 50% and 95% posterior intervals for each b estimate
#' @examples
#' item_mcmc_intervals(im_model)
#' @export
item_mcmc_intervals = function(stan_model_output){
  intervals = mcmc_intervals_data(as.array(stan_model_output),
                                  regex_pars = 'b',
                                  prob_outer = .95) %>%
    mutate(., exID = as.integer(substr(parameter, 7, nchar(as.character(parameter)) - 1)))
  return(intervals)
}
