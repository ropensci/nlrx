# Postprocessing


nlrx_attach_results <- function(nl, results) {

  library(tibble)

  nl@experiment@simdesign@simresults <- as.tibble(results)

  return(nl)
}




nlrx_postpro <- function(nl) {

  method <- nl@experiment@simdesign@method

  if (method == "lhs") {
    out <- nlrx_postpro_lhs(nl)
  }
  if (method == "morris") {
    out <- nlrx_postpro_morris(nl)
  }
  if (method == "sobol") {
    out <- nlrx_postpro_sobol(nl)
  }

  return(out)
}



nlrx_postpro_lhs <- function(nl) {

}



nlrx_postpro_sobol <- function(nl) {


  simresults <- nl@experiment@simdesign@simresults

#
#   ## Remove NaNs from sim.results.morris
#   sim.results.sobol[is.na(sim.results.sobol)] <- 0
#
#   ## STORE Raw Data from Morris simus:
#   sim.output <- data.frame(t(sim.results.sobol))
#   names(sim.output) <- output.names
#   sim.input.output <- cbind(so$X, sim.output)
#   raw_data <- rbind(raw_data, sim.input.output)
#
#   ## Store effects data in dataframe:
#   for (i in (1:length(sim.results.sobol[,1])))
#   {
#     tell(so, sim.results.sobol[i,])
#     ss <- so$S
#     ss[ss < 0] <- 0
#     ss[ss > 1] <- 1
#     ss$param <- rownames(ss)
#     ss$output <- output.names[i]
#     ss$seed <- new.model.seed
#
#     final_data <- rbind(final_data, ss, tt)
#   }
#

}


nlrx_postpro_morris <- function(nl) {

  #
  # ## We can set the carbon values to zero for those runs:
  # sim.results.morris[is.na(sim.results.morris)] <- 0
  #
  # ## STORE Raw Data from Morris simus:
  # sim.output <- data.frame(t(sim.results.morris))
  # names(sim.output) <- output.names
  # sim.input.output <- cbind(mo$X, sim.output)
  # raw_data <- rbind(raw_data, sim.input.output)
  #
  # ## Once the matrix is finished, do basic analysis and convert it to dataframe:
  # for (l in (1:length(sim.results.morris[,1])))
  # {
  #   tell(mo, sim.results.morris[l,])
  #
  #   ##Calculate effects mu.star, mu and sigma
  #   mu.star <- apply(mo$ee, 2, function(x) mean(abs(x)))
  #   mu <- apply(mo$ee, 2, mean)
  #   sigma <- apply(mo$ee, 2, sd)
  #
  #   ##Create dataframes with parameter and output names:
  #   mu.star_df <- data.frame(morrisrun=k, output=output.names[l], parameter=rownames(as.data.frame(mu.star)), effect="mu.star", value=mu.star, seed=new.model.seed)
  #   mu_df <- data.frame(morrisrun=k, output=output.names[l], parameter=rownames(as.data.frame(mu)), effect="mu", value=mu, seed=new.model.seed)
  #   sigma_df <- data.frame(morrisrun=k, output=output.names[l], parameter=rownames(as.data.frame(sigma)), effect="sigma", value=sigma, seed=new.model.seed)
  #
  #   ##Bind data together:
  #   final_data <- rbind(final_data, mu.star_df, mu_df, sigma_df)
  # }
}
