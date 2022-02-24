# lognormal-experiment (in progress)
Test to see how assumptions about lognormal biomass data affect design-based (DB) indices of abundance.

## Steps
1. Using POP and arrowtooth as an example, find strata that suit the following conditions:
  - similar stratum biomass, similar CV
  - similar stratum biomass, different CV
  - different stratum biomass, similar CV
  
2. Bootstrap the hauls for those strata, calculating DB stratum-level biomass each time
3. Compare the bootstrapped stratum biomasses to a lognormal distribution
