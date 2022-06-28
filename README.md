# lognormal-experiment (in progress)
Test to see how assumptions about log-normality of stratum biomass affect design-based (DB) indices of abundance. This code produces bootstrapped design-based estimates of stratum-level biomass.

## Steps
1. Bootstrap hauls from each stratum with replacement (for each bootstrap, sample 100% of the hauls in each stratum with replacement). Default bootstrap size is `n=1000` 
2. For each bootstrapped dataset, calculate stratum-level biomass
4. From stratum biomasses, calculate total survey area biomass
5. Compare that to a lognormal distribution

## Outputs
Outputs are [here](https://drive.google.com/drive/folders/1oO3zmskOeuXaEQ15uRjgkh_LEZ0h6xV8?usp=sharing). They include:

* Resampled CPUEs for each stratum
* Stratum biomass calculated for each bootstrap
* Total survey area biomass for each bootstrap
* Summary of differences between bootstrap variances and the variances we calculated using the method in Wakabayashi et al.



## Manuscript drafts
Manuscript draft (Monnahan et al. 20XX) is [here](https://docs.google.com/document/d/1g0jH5b1iw7Y9wjfN4yOBF7K5Uo1Lw5IuM1LCw2oZykU/edit?usp=sharing).

## NOAA README
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

