# idea-screening


Code for running cross validation and creating the Idea Screening Efficiency curve from Bell, Pescher, and Tellis 2023

## Variables

| Variable Name                                             | Name in Script                  |
|-----------------------------------------------------------|---------------------------------|
| Shortlisted (yes/no)                                      | `shortlist (1/0)`               |
| Winner (1st/2nd/3rd)                                      | `winner (1/2/3)`                |
| Contest Name                                              | `contest`                       |
| Word Atypicality                                          | `peer_dev_jacc`                 |
| Topic Atypicality                                         | `peer_dev_lda`                  |
| Prototypicality                                           | `ks_goog`                       |
| Word Count                                                | `wc`                            |
| Min, Max, Avg. Node Freq.                                 | `min_nf`, `max_nf`, `avg_nf`    |
| Min, Max, Avg. Jaccard Index                              | `min_jm`, `max_jm`, `avg_jm`    |
| Node Freq. Coef. of Variation                             | `nf_coef_var`                   |
| Jaccard Index Coef. of Var.                               | `jm_coef_var`                   |
| Constraints Metric.                                       | `burt`                          |
| Clustering Coefficient                                    | `cc_orig`                       |
| Clustering Coefficient Outdegree Only                     | `cc`                            |
| Degree                                                    | `degree`                        |

## Important Note
We have noticed that results can vary from system to system. The changes are small but seem to depend on the versions of the installed `R` libraries. The results in `Output` were generated with current version of the scripts, and the set of `R` packages in the file `package_state.csv`. 

Results will also vary depending on the corpora used to construct some variables, such as `ks_goog`. We have not provided code used to collect Google results, as the method is unlikely to be stable. 
