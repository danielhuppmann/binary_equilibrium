Nash equilibria in binary strategies
====================================

Abstract
--------
We propose a novel method to find Nash equilibria in games with binary decision variables 
by including compensation payments and incentive-compatibility constraints from non-cooperative game theory 
directly into an optimization framework in lieu of using first order conditions of a linearization, 
or relaxation of integrality conditions. 

The reformulation offers a new approach to obtain and interpret dual variables to binary constraints 
using the benefit or loss from deviation rather than marginal relaxations. 
The method endogenizes the trade-off between overall (societal) efficiency 
and compensation payments necessary to align incentives of individual players.

The manuscript with the theoretical background of dual variables in integer programs 
and the mathematical explanation of our method is published 
in the [European Journal of Operational Research](https://doi.org/10.1016/j.ejor.2017.09.032).
Preprints and working versions can be downloaded 
from [arXiv](http://arxiv.org/abs/1504.05894) 
and [OptimizationOnline](http://www.optimization-online.org/DB_HTML/2015/04/4874.html).

Content
-------
This repository contains the GAMS codes for the numerical results for the electricity market example 
presented in Chapter 4 and the Appendix of the manuscript (including all data), 
as well as an additional illustrative example of a natural gas investment and operation game.

- ``MOPBQE_power_market_example``: 
  A stylized electricity network with 6 nodes, 9 generations and 2 time periods
  to illustrate the binary equilibrium method. The results are presented in Chapter 4 of the manuscript. <br />
  Data modified from Example 5.1 in
  Steven Gabriel, Antonio Conejo, Carlos Ruiz & Sauleh Siddiqui.
  "Solving discretely-constrained, Mixed Complementarity Problems with Applications in Energy",
  *Computers & Operations Research*, 40(5):1339-1350, 2013.
  [DOI: 10.1016/j.cor.2012.10.017](http://dx.doi.org/10.1016/j.cor.2012.10.017).
  
- ``MOPBQE_power_market_numerical_tests``: 
  A large-scale electricity network dataset for the numerical tests 
  presented in the Appendix of the manuscript. <br />
  Data based on:
  H. Pandzic, Y. Dvorkin, T. Qiu, Y. Wang, and D. Kirschen.
  Unit Commitment under Uncertainty - GAMS Models
  Library of the Renewable Energy Analysis Lab (REAL)
  University of Washington, Seattle, USA.
  http://www.ee.washington.edu/research/real/gams_code.html.

 - ``MOPBQE_resource_market_example``: 
   A stylized natural gas investment and operation game to illustrate the versatility
   of the binary-equilibrium approach and its applicability to problems
   beyond the electricity sector.

Bibliography info
-----------------
*Please cite as:* <br />
Daniel Huppmann and Sauleh Siddiqui.
"An exact solution method for binary equilibrium problems with compensation and the power market uplift problem",
*European Journal of Operational Research*, 266(2):622-638, 2018, 
[DOI: 10.1016/j.ejor.2017.09.032](https://doi.org/10.1016/j.ejor.2017.09.032).

Classification
--------------
### Keywords
binary Nash game, non-cooperative equilibrium, multi-objective optimisation, 
compensation, incentive compatibility, electricity market, power market, uplift payments

### Journal of Economic Literature Classification [JEL Codes](https://www.aeaweb.org/econlit/jelCodes.php?view=jel)
C72, C61, L13, L94

### Mathematics Subject Classification [MSC](https://cran.r-project.org/web/classifications/MSC.html)
90C11, 90C46, 91B26

Authors and Contributors
------------------------
This method and the codes were developed by @danielhuppmann and @ssaul3h.

License
-------
This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/)
