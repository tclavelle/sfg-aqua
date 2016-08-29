##############################################################
##
## Functions script for aquaculture-fisheries model
##
##############################################################

# r = 1.5
# p = 850
# c = 20
# q = 0.0001
# Kmax = 1000
# phi = 0.5
# A = 300
# delta = 0.05

biomassGrowth <- function(r, B, K) {
  b_Growth <- r * B * (1 -  B / K)
  return(b_Growth)
}

carryingCapacity <- function(Kmax, phi, A) {
  K <- Kmax - phi * A
  return(K)
}

fisheryHarvest <- function(q, B, E) {
  h_f <- q * B * E
  return(h_f)
}

stockGrowth <- function(r, B, Kmax, phi, A, q, E) {
  B_out <- r * B * (1 -  B / (Kmax - phi * A)) - (q * B * E)
  return(B_out)
}

optimalStock <- function(K, c, p, delta, r) {
  B_star <- K / 4 * ((c/(p*q*K) + 1 - delta/r) + ((c/(p*q*K) + 1 - delta/r)^2 + (8*c*delta) / (p*q*r*K))^0.5)
  return(B_star)
}

quotaPrice <- function(K, p, c, q, B) {
  q_value <- p - (c/(q*B*K))
  return(q_value)
}

demandFunc <- function(choke_p, h_f, h_a, slope) {
  p_out <- choke_p - slope * (h_f + h_a)
}
