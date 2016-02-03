$TITLE Equilibrium problems with binary decision variables - Naturas gas market and investment game example

$ONTEXT
Daniel Huppmann, Sauleh Siddiqui
Johns Hopkins University, International Institute for Applied Systems Analysis (IIASA), DIW Berlin
huppmann (at) iiasa.ac.at, siddiqui (at) jhu.edu

Application of a multi-objective program subject to a binary quasi-equilibrium between non-cooperative players
to an example of an investment and production game applied to the natural gas market

This code and illustrative dataset was prepared as supplementary material to the submission of the manuscript
"An exact solution method for binary equilibrium problems with compensation and the power market uplift problem",
published as DIW Discussion Paper 1475, 2015 (http://diw.de/sixcms/detail.php?id=diw_01.c.502763.de)
and posted on arXive (http://arxiv.org/abs/1504.05894)

Please cite as:
Daniel Huppmann and Sauleh Siddiqui.
"An exact solution method for binary equilibrium problems with compensation and the power market uplift problem",
DIW Discussion Paper 1475, 2015.

This work is licensed under a Creative Commons Attribution 4.0 International License
-> http://creativecommons.org/licenses/by/4.0/

For more information and applications of binary equilibrium problems, please visit:
-> https://www.github.com/danielhuppmann/binary_equilibrium

Version: January 28, 2016
$OFFTEXT

$EOLCOM #
option MIQCP = GUROBI ;

************************************************************************************************************************
*** Sets and parameters                                                                                              ***
************************************************************************************************************************

*** Sets ***

Sets
	n nodes	/ n1*n3 /
	t time	/ t1*t2 /
	i firms	/ i1*i3 /
	l arcs	/ l1*l2 /	# pipeline connections
;

Alias (n,m) ;
Alias (n,nn) ;
Alias (l,ll) ;

*** Parameters ***

Parameters
* production parameters
	p_X_inv(i,n)	investment cost in production capacity at node n (lump-sum)
	p_Z_inv(i,n)	investment cost in production capacity at node n (per unit of capacity)
	p_Z_max(i,n)	maximum production capacity investment (if binary production capacity investment x_P = 1)
	p_Z_min(i,n)	minimum production capacity investment (if binary production capacity investment x_P = 1)
	p_Y_mc(t,i,n)	marginal cost of production (per unit)
	p_Y_max(i,n)	initial maximum production capacity (without additional investment)
	p_Y_min(i,n)	minimum production capacity

* pipeline parameters
	a_X_inv(i,l)	investment cost in pipeline capacity for arc l (lump-sum)
	a_Z_inv(i,l)	investment cost in pipeline capacity for arc l (per unit of capacity)
	a_Z_max(i,l)	maximum pipeline capacity investment (if binary arc capacity investment x_A = 1)
	a_Z_min(i,l)	minimum pipeline capacity investment (if binary arc capacity investment x_A = 1)
	a_Y_mc(t,i,l)	marginal cost of using arc l (per unit)
	a_Y_max(i,l)	initial maximum pipeline capacity (without additional investment)

* access and pipeline grid mapping
	grid_A(l,n,m)	mapping of pipeline network (grid) node-to-node
	map_P(i,n)	mapping of firm to production nodes
	map_A(i,l)	mapping of firm to pipeline
	map_N(i,n)	mapping of firm to node (production - transit - sales)

* demand function parameters
	int_D(t,n)	intercept of inverse demand function
	slp_D(t,n)	slope of inverse demand function
;

************************************************************************************************************************
*** Input data                                                                                                       ***
************************************************************************************************************************

*** demand side ***
	int_D('t1','n3') = 130 ;
	slp_D('t1','n3') = 1 ;
	int_D('t2','n3') = 90 ;
	slp_D('t2','n3') = 1 ;

*** pipeline network mapping ***
	grid_A('l1','n1','n3') = 1 ;
	grid_A('l2','n2','n3') = 1 ;

*** firm 1 ***
	map_P('i1','n1') = 1 ;
	p_X_inv('i1','n1') = 20 ;
	p_Z_inv('i1','n1') = 20 ;
	p_Z_max('i1','n1') = 30 ;
	p_Z_min('i1','n1') = 20 ;
	p_Y_mc(t,'i1','n1') = 20 ;
	p_Y_max('i1','n1') = 30 ;
	p_Y_min('i1','n1') = 10 ;

	map_A('i1','l1') = 1 ;
	a_X_inv('i1','l1') = 15 ;
	a_Z_inv('i1','l1') = 20 ;
	a_Z_max('i1','l1') = 30 ;
	a_Z_min('i1','l1') = 15 ;
	a_Y_mc(t,'i1','l1') = 10 ;
	a_Y_max('i1','l1') = 20 ;

	map_N('i1','n1') = 1 ;
	map_N('i1','n3') = 1 ;

*** firm 2 ***
	map_P('i2','n2') = 1 ;
	p_X_inv('i2','n2') = 10 ;
	p_Z_inv('i2','n2') = 15 ;
	p_Z_max('i2','n2') = 20 ;
	p_Z_min('i2','n2') = 10 ;
	p_Y_mc(t,'i2','n2') = 25 ;
	p_Y_max('i2','n2') = 20 ;
	p_Y_min('i2','n2') = 10 ;

	map_A('i2','l2') = 1 ;
	a_X_inv('i2','l2') = 10 ;
	a_Z_inv('i2','l2') = 10 ;
	a_Z_max('i2','l2') = 20 ;
	a_Z_min('i2','l2') = 10 ;
	a_Y_mc(t,'i2','l2') = 10 ;
	a_Y_max('i2','l2') = 20 ;

	map_N('i2','n2') = 1 ;
	map_N('i2','n3') = 1 ;

*** firm 3 ***
	map_P('i3','n3') = 1 ;
	p_X_inv('i3','n3') = 30 ;
	p_Z_inv('i3','n3') = 20 ;
	p_Z_max('i3','n3') = 20 ;
	p_Z_min('i3','n3') = 10 ;
	p_Y_mc(t,'i3','n3') = 40 ;
	p_Y_max('i3','n3') = 0 ;
	p_Y_min('i3','n3') = 0 ;

	map_N('i3','n3') = 1 ;

*** Permutations of binary strategies ***

Acronyms OO, pO, Oa, pa ;

Set phi	/ 00, p0, 0a, pa / ;

Alias (phi, phi2) ;

Parameters
	phi_I(i,phi)		mapping whether strategy phi is available to firm i
	x_P_phi(i,n,phi)	binary production investment strategy in scenario phi
	x_A_phi(i,l,phi)	binary pipeline investment strategy in scenario phi
;

* the 00 strategy means no investment anywhere for firm i
phi_I(i,'00') = 1 ;
x_P_phi(i,n,'00') = 0 ;
x_A_phi(i,l,'00') = 0 ;

* the p0 strategy means production capacity investment at the respective firms production node
phi_I(i,'p0') = 1 ;
x_P_phi('i1','n1','p0') = 1 ;
x_P_phi('i2','n2','p0') = 1 ;
x_P_phi('i3','n3','p0') = 1 ;
x_A_phi(i,l,'p0') = 0 ;

* the 0a strategy means arc capacity investment at the respective firms production node
* note that firm i3 does not have a access to an arc, so this strategy does not apply for that firm
phi_I('i1','0a') = 1 ;
x_P_phi('i1','n1','0a') = 0 ;
x_A_phi('i1','l1','0a') = 1 ;
phi_I('i2','0a') = 1 ;
x_P_phi('i2','n2','0a') = 0 ;
x_A_phi('i2','l2','0a') = 1 ;

* the pa strategy means both production capacity and arc capacity investment where possible
* note that firm i3 does not have a access to an arc, so this strategy does not apply for that firm
phi_I('i1','pa') = 1 ;
x_P_phi('i1','n1','pa') = 1 ;
x_A_phi('i1','l1','pa') = 1 ;
phi_I('i2','pa') = 1 ;
x_P_phi('i2','n2','pa') = 1 ;
x_A_phi('i2','l2','pa') = 1 ;

* note that if a firm could invest in production or arc capacity at multiple nodes, the number of binary strategies
* would increase exponentially

* the big M (which is actually an K) for the  disjunctive constraints reformulation
Parameters
* disjunctive constraints reformulation for the new-dual and compensation payments
	K_compensation 	large scalar for compensation payment constraint	/ 100000 /
;

*** Output report parameters ***

Parameters report_summary(*,*,*), report_wf(*,*,*,*,*), report_dev(*,*,*,*) ;

************************************************************************************************************************
*** Definition of variables and equations                                                                            ***
************************************************************************************************************************

*** Variables ***

Variables
	obj		objective value of upper-level objective function
;

Binary variables
	x_P(i,n)	binary investment decision of firm i in production capacity at node n
	x_A(i,l)	binary investment decision of firm in pipeline capacity on arc l
	x_phi(i,phi)	binary strategy decision for firm i (denotes a specific permutation of decisions x_P and x_A)
;

Positive variables
	z_P(i,n)	capacity investment decision of firm i in production capacity at node n
	z_A(i,l)	capacity expansion decision of firm i in pipeline capacity of arc l
	y_P(t,i,n)	production decision of firm i at node n in time t
	y_A(t,i,l)	transport decision of firm i at node n in time t
	y_S(t,i,n)	sales decision to final demand of firm i at node n in time t

	gamma(t,i,n)	dual to nodal energy balance constraint of firm i
	rho(i,n)	dual to maximum production capacity investment
	theta(i,n)	dual to minimum production capacity investment
	beta(t,i,n)	dual to maximum capacity constraint for firm i
	alpha(t,i,n)	dual to minimum activity constraint for firm i
	eta(i,l)	dual to maximum pipeline capacity investment
	nu(i,l)		dual to minimum pipeline capacity investment
	mu(t,i,l)	dual to pipeline capacity constraint

	z_P_phi(i,n,phi)	capacity investment decision of firm i in binary strategy phi
	z_A_phi(i,l,phi)	capacity expansion decision of firm i in binary strategy phi
	y_P_phi(t,i,n,phi)	production decision of firm i in binary strategy phi
	y_A_phi(t,i,l,phi)	transport decision of firm i in binary strategy phi
	y_S_phi(t,i,n,phi)	sales decision to final demand of firm i in binary strategy phi

	gamma_phi(t,i,n,phi)	dual to nodal energy balance constraint of firm i in binary strategy phi
	rho_phi(i,n,phi)	dual to maximum production capacity investment in binary strategy phi
	theta_phi(i,n,phi)	dual to minimum production capacity investment in binary strategy phi
	beta_phi(t,i,n,phi)	dual of maximum capacity constraint in binary strategy phi
	alpha_phi(t,i,n,phi)	dual of minimum activity constraint in binary strategy phi
	eta_phi(i,l,phi)	dual to maximum pipeline capacity investment
	nu_phi(i,l,phi)		dual to minimum pipeline capacity investment
	mu_phi(t,i,l,phi)	dual to pipeline capacity constraint in binary strategy phi

	d(t,n)			demand by consumers at node n
	price(t,n)		market price (dual to market-clearing constraint)

	kappa_act_profit(i,phi)		profit in binary strategy phi if strategy phi is chosen
	kappa_act_loss(i,phi)		loss in binary strategy phi if strategy phi is chosen
	kappa_inact_profit(i,phi)	profit in binary strategy phi if strategy phi is NOT chosen
	kappa_inact_loss(i,phi)		loss in binary strategy phi if strategy phi is NOT chosen

	zeta(i)		compensation payments to guarantee incentive compatibility
;

* binary variables for disjunctive constraints reformulation
Binary variables

* standard formulation
	r_stat_y_A(t,i,l)
	r_CON_z_P_max(i,n)
	r_CON_z_P_min(i,n)
	r_CON_y_P_max(t,i,n)
	r_CON_y_P_min(t,i,n)
	r_CON_z_A_max(i,l)
	r_CON_z_A_min(i,l)
	r_CON_y_A_max(t,i,l)

* binary equilibrium
	r_stat_y_A_phi(t,i,l,phi)
	r_CON_z_P_max_phi(i,n,phi)
	r_CON_z_P_min_phi(i,n,phi)
	r_CON_y_P_max_phi(t,i,n,phi)
	r_CON_y_P_min_phi(t,i,n,phi)
	r_CON_z_A_max_phi(i,l,phi)
	r_CON_z_A_min_phi(i,l,phi)
	r_CON_y_A_max_phi(t,i,l,phi)
;

*** Definition of equations ***

Equations
	obj_simple	objective function of overall problem (welfare-optimal)
	obj_binary_equilibrium	objective function of overall problem (binary equilibrium problem)
* stationarity conditions of demand and market clearing constraint
	KKT_D		stationarity condition (KKT) for demand
	MCC		market-clearing constraint
* first-order optimality conditions for the firms - standard formulation
	KKT_z_P 	stationarity condition for production capacity investment
	KKT_y_P		stationarity condition for production quantity decision
	KKT_z_A		stationarity condition for pipeline quantity decision
	KKT_y_A		stationarity condition for pipeline quantity decision
	KKT_y_A_DC1, KKT_y_A_DC2
	KKT_y_S		stationarity condition for sales to consumers
* first-order optimality conditions for the firms - binary equilibrium formulation
	KKT_z_P_phi 	stationarity condition for production capacity investment in binary strategy phi
	KKT_y_P_phi	stationarity condition for production quantity decision in binary strategy phi
	KKT_z_A_phi	stationarity condition for pipeline quantity decision in binary strategy phi
	KKT_y_A_phi	stationarity condition for pipeline quantity decision in binary strategy phi
	KKT_y_A_phi_DC1, KKT_y_A_phi_DC2
	KKT_y_S_phi_Scarf		stationarity condition for sales in binary strategy phi given current prices
	KKT_y_S_phi_anticipation	stationarity condition for sales considering own impact on equilibrium price
* Constraints for the firms - standard formulation
	CON_MBC		nodal mass-balance constraint (by firm-node)
	CON_z_P_max	maximum production capacity investment constraint (if active)
	CON_z_P_max_DC1, CON_z_P_max_DC2
	CON_z_P_min	minimum production capacity investment constraint (if active)
	CON_z_P_min_DC1, CON_z_P_min_DC2
	CON_y_P_max	maximum production capacity constraint
	CON_y_P_max_DC1, CON_y_P_max_DC2
	CON_y_P_min	minimum production capacity constraint
	CON_y_P_min_DC1, CON_y_P_min_DC2
	CON_z_A_max	maximum pipeline capacity investment constraint (if active)
	CON_z_A_max_DC1, CON_z_A_max_DC2
	CON_z_A_min	minimum pipeline capacity investment constraint (if active)
	CON_z_A_min_DC1, CON_z_A_min_DC2
	CON_y_A_max	maximum pipeline capacity utilization constraint
	CON_y_A_max_DC1, CON_y_A_max_DC2
* constraints for the firms - binary equilibrium formulation
	CON_MBC_phi	nodal mass-balance constraint in binary strategy phi
	CON_z_P_max_phi	maximum production capacity investment constraint in binary strategy phi
	CON_z_P_max_phi_DC1, CON_z_P_max_phi_DC2
	CON_z_P_min_phi	minimum production capacity investment constraint in binary strategy phi
	CON_z_P_min_phi_DC1, CON_z_P_min_phi_DC2
	CON_y_P_max_phi	maximum production capacity constraint in binary strategy phi
	CON_y_P_max_phi_DC1, CON_y_P_max_phi_DC2
	CON_y_P_min_phi	minimum production capacity constraint in binary strategy phi
	CON_y_P_min_phi_DC1, CON_y_P_min_phi_DC2
	CON_z_A_max_phi	maximum pipeline capacity investment constraint in binary strategy phi
	CON_z_A_max_phi_DC1, CON_z_A_max_phi_DC2
	CON_z_A_min_phi	minimum pipeline capacity investment constraint in binary strategy phi
	CON_z_A_min_phi_DC1, CON_z_A_min_phi_DC2
	CON_y_A_max_phi	maximum pipeline capacity utilization constraint in binary strategy phi
	CON_y_A_max_phi_DC1, CON_y_A_max_phi_DC2
* incentive compatibility constraints
	PROFIT_PHI	linear reformulation of profit in binary strategy phi
	INCENTIVE_COMP	incentive compatibility constraint (no profitable deviation exists)
	KAPPA_ACTIVE	assignment constraint to ensure that the kappa for the active strategies are correctly assigned
	KAPPA_INACTIVE	assignment constraint to ensure that the kappa for inactive strategies are correctly assigned
	CON_x_phi	constraint that exactly one binary strategy is chosen
* translation of optimal decision into equilibrium outcome and variable "seen" by rivals
	TRANS_z_P_phi_ge
	TRANS_z_P_phi_le
	TRANS_z_A_phi_ge
	TRANS_z_A_phi_le
	TRANS_y_P_phi_ge
	TRANS_y_P_phi_le
	TRANS_y_A_phi_ge
	TRANS_y_A_phi_le
	TRANS_y_S_phi_ge
	TRANS_y_S_phi_le
;

************************************************************************************************************************
*** Declaration of equations                                                                                         ***
************************************************************************************************************************

*** Objective functions ***

* standard objective - welfare maximization
* consumer surplus less costs (production, transport, infrastructure investment)

* standard formulation
obj_simple..
	obj =e= sum((t,n)$int_D(t,n), ( int_D(t,n) - 0.5 * slp_D(t,n) * d(t,n) ) * d(t,n) )
		- sum((i,n), p_X_inv(i,n) * x_P(i,n) + p_Z_inv(i,n) * z_P(i,n)
			+ sum(t, p_Y_mc(t,i,n) * y_P(t,i,n) ) )
		- sum((i,l), a_X_inv(i,l) * x_A(i,l) + a_Z_inv(i,l) * z_A(i,l)
			+ sum(t, a_Y_mc(t,i,l) * y_A(t,i,l) ) )
;

* binary equilibrium objective with compensation
obj_binary_equilibrium..
	obj =e= sum((t,n)$int_D(t,n), ( int_D(t,n) - 0.5 * slp_D(t,n) * d(t,n) ) * d(t,n) )
		- sum((i,phi), x_phi(i,phi) *
			sum(n, p_X_inv(i,n) * x_P_phi(i,n,phi) )
			+ sum(l, a_X_inv(i,l) * x_A_phi(i,l,phi) ) )
		- sum((i,n), p_Z_inv(i,n) * z_P(i,n) + sum(t, p_Y_mc(t,i,n) * y_P(t,i,n) ) )
		- sum((i,l), a_Z_inv(i,l) * z_A(i,l) + sum(t, a_Y_mc(t,i,l) * y_A(t,i,l) ) )
		- sum(i, zeta(i) )
;

*** stationarity (KKT) condition for demand ***

KKT_D(t,n)$int_D(t,n)..								# dual to d
	- int_D(t,n) + slp_D(t,n) * d(t,n) + price(t,n) =E= 0 ;

*** market-clearing constraint ***

MCC(t,n)$int_D(t,n)..
	d(t,n) - sum(i, y_S(t,i,n) ) =E= 0 ;					# dual to price

*** first-order optimality (KKT) conditions (by firm) ***

* standard formulation

KKT_z_P(i,n)$map_P(i,n)..							# dual to z_P
	p_Z_inv(i,n) + rho(i,n) - theta(i,n) - sum(t, beta(t,i,n) ) =E= 0 ;

KKT_y_P(t,i,n)$map_P(i,n)..							# dual to y_P
	p_Y_mc(t,i,n) + beta(t,i,n) - alpha(t,i,n) - gamma(t,i,n) =E= 0 ;

KKT_z_A(i,l)$map_A(i,l)..							# dual to z_A
	 a_Z_inv(i,l) + eta(i,l) - nu(i,l) - sum(t, mu(t,i,l) ) =E= 0 ;

* note that there is no explicit lower bound on the quantity transported (y_A non-negative)
KKT_y_A(t,i,l)$map_A(i,l)..							# dual to y_A
	a_Y_mc(t,i,l) + mu(t,i,l)
	+ sum((n,m)$( grid_A(l,n,m) ), gamma(t,i,n) - gamma(t,i,m) ) =G= 0 ;

KKT_y_A_DC1(t,i,l)$map_A(i,l)..
	a_Y_mc(t,i,l) + mu(t,i,l)
	+ sum((n,m)$( grid_A(l,n,m) ), gamma(t,i,n) - gamma(t,i,m) )
	=L= 1000 * r_stat_y_A(t,i,l) ;

KKT_y_A_DC2(t,i,l)$map_A(i,l)..
	y_A(t,i,l) =L= ( a_Y_max(i,l) + a_Z_max(i,l) ) * 1.1 * ( 1 - r_stat_y_A(t,i,l) ) ;

KKT_y_S(t,i,n)$int_D(t,n)..							# dual to y_S
	gamma(t,i,n) - price(t,n) =E= 0 ;

* binary equilibrium formulation

KKT_z_P_phi(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..				# dual to z_P_phi
	p_Z_inv(i,n) + rho_phi(i,n,phi) - theta_phi(i,n,phi) - sum(t, beta_phi(t,i,n,phi) ) =E= 0 ;

KKT_y_P_phi(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..			# dual to y_P_phi
	p_Y_mc(t,i,n) + beta_phi(t,i,n,phi) - alpha_phi(t,i,n,phi) - gamma_phi(t,i,n,phi) =E= 0 ;

KKT_z_A_phi(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..				# dual to z_A_phi
	 a_Z_inv(i,l) + eta_phi(i,l,phi) - nu_phi(i,l,phi) - sum(t, mu_phi(t,i,l,phi) ) =E= 0 ;

* note that there is no explicit lower bound on the quantity transported (y_A non-negative)
KKT_y_A_phi(t,i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..			# dual to y_A_phi
	a_Y_mc(t,i,l) + mu_phi(t,i,l,phi)
	+ sum((n,m)$( grid_A(l,n,m) ), gamma_phi(t,i,n,phi) - gamma_phi(t,i,m,phi) ) =G= 0 ;

KKT_y_A_phi_DC1(t,i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	a_Y_mc(t,i,l) + mu_phi(t,i,l,phi)
	+ sum((n,m)$( grid_A(l,n,m) ), gamma_phi(t,i,n,phi) - gamma_phi(t,i,m,phi) )
	=L= 1000 * r_stat_y_A_phi(t,i,l,phi) ;

KKT_y_A_phi_DC2(t,i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	y_A_phi(t,i,l,phi) =L= ( a_Y_max(i,l) + a_Z_max(i,l) ) * 1.1 * ( 1 - r_stat_y_A_phi(t,i,l,phi) ) ;

* this is the first-order condition given prices resulting in equilibrium (Scarf's condition)
KKT_y_S_phi_Scarf(t,i,n,phi)$( phi_I(i,phi) AND int_D(t,n) )..			# dual to y_S_phi
	gamma_phi(t,i,n,phi) - price(t,n) =E= 0 ;

* this is the first-order condition given prices if firm i were to deviate from the equilibrium outcome
* note that this is not market power in the Cournot sense, only an anticipation of the resulting competitive outcome
KKT_y_S_phi_anticipation(t,i,n,phi)$( phi_I(i,phi) AND int_D(t,n) )..		# dual to y_S_phi
	gamma_phi(t,i,n,phi) - int_D(t,n) + slp_D(t,n) * ( d(t,n) - y_S(t,i,n) + y_S_phi(t,i,n,phi) ) =E= 0 ;

*** nodal mass-balance constraint (by firm) ***

* standard formulation

CON_MBC(t,i,n)$( map_N(i,n) )..							# dual to gamma
* domestic production
	y_P(t,i,n)$map_P(i,n)
* exports from that node
	+ sum((l,m)$( map_A(i,l) AND grid_A(l,m,n) ), y_A(t,i,l) )
* imports to that node
	- sum((l,m)$( map_A(i,l) AND grid_A(l,n,m) ), y_A(t,i,l) )
* domestic sales
	- y_S(t,i,n)$int_D(t,n) =E= 0 ;

* binary equilibrium formulation

CON_MBC_phi(t,i,n,phi)$( phi_I(i,phi) AND map_N(i,n) )..			# dual to gamma_phi
* domestic production
	y_P_phi(t,i,n,phi)$map_P(i,n)
* exports from that node
	+ sum((l,m)$( map_A(i,l) AND grid_A(l,m,n) ), y_A_phi(t,i,l,phi) )
* imports to that node
	- sum((l,m)$( map_A(i,l) AND grid_A(l,n,m) ), y_A_phi(t,i,l,phi) )
* domestic sales
	- y_S_phi(t,i,n,phi)$int_D(t,n) =E= 0 ;

*** maximum/minimum production capacity investment constraint (if active) ***

* standard formulation

CON_z_P_max(i,n)$map_P(i,n)..							# dual to rho
	p_Z_max(i,n) * x_P(i,n) - z_P(i,n) =G= 0 ;

CON_z_P_max_DC1(i,n)$map_P(i,n)..
	p_Z_max(i,n) * x_P(i,n) - z_P(i,n) =L= p_Z_max(i,n) * 1.1 * r_CON_z_P_max(i,n) ;

CON_z_P_max_DC2(i,n)$map_P(i,n)..
	rho(i,n) =L= 1000 * ( 1 - r_CON_z_P_max(i,n) ) ;

* binary equilibrium formulation

CON_z_P_max_phi(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..			# dual to rho_phi
	p_Z_max(i,n) * x_P_phi(i,n,phi) - z_P_phi(i,n,phi) =G= 0 ;

CON_z_P_max_phi_DC1(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	p_Z_max(i,n) * x_P_phi(i,n,phi) - z_P_phi(i,n,phi) =L= p_Z_max(i,n) * 1.1 * r_CON_z_P_max_phi(i,n,phi) ;

CON_z_P_max_phi_DC2(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	rho_phi(i,n,phi) =L= 1000 * ( 1 - r_CON_z_P_max_phi(i,n,phi) ) ;

* standard formulation

CON_z_P_min(i,n)$map_P(i,n)..							# dual to theta
	- p_Z_min(i,n) * x_P(i,n) + z_P(i,n) =G= 0 ;

CON_z_P_min_DC1(i,n)$map_P(i,n)..
	- p_Z_min(i,n) * x_P(i,n) + z_P(i,n) =L= p_Z_max(i,n) * 1.1 * r_CON_z_P_min(i,n) ;

CON_z_P_min_DC2(i,n)$map_P(i,n)..
	theta(i,n) =L= 1000 * ( 1 - r_CON_z_P_min(i,n) ) ;

* binary equilibrium formulation

CON_z_P_min_phi(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..			# dual to theta_phi
	- p_Z_min(i,n) * x_P_phi(i,n,phi) + z_P_phi(i,n,phi) =G= 0 ;

CON_z_P_min_phi_DC1(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	- p_Z_min(i,n) * x_P_phi(i,n,phi) + z_P_phi(i,n,phi) =L= p_Z_max(i,n) * 1.1 * r_CON_z_P_min_phi(i,n,phi) ;

CON_z_P_min_phi_DC2(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	theta_phi(i,n,phi) =L= 1000 * ( 1 - r_CON_z_P_min_phi(i,n,phi) ) ;

*** maximum/minimum production capacity constraint ***

* standard formulation

CON_y_P_max(t,i,n)$map_P(i,n)..							# dual to beta
	p_Y_max(i,n) + z_P(i,n) - y_P(t,i,n) =G= 0 ;

CON_y_P_max_DC1(t,i,n)$map_P(i,n)..
	p_Y_max(i,n) + z_P(i,n) - y_P(t,i,n) =L= ( p_Y_max(i,n) + p_Z_max(i,n) ) * 1.1 * r_CON_y_P_max(t,i,n) ;

CON_y_P_max_DC2(t,i,n)$map_P(i,n)..
	beta(t,i,n) =L= 1000 * ( 1 - r_CON_y_P_max(t,i,n) ) ;

* binary equilibrium formulation

CON_y_P_max_phi(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..			# dual to beta_phi
	p_Y_max(i,n) + z_P_phi(i,n,phi) - y_P_phi(t,i,n,phi) =G= 0 ;

CON_y_P_max_phi_DC1(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	p_Y_max(i,n) + z_P_phi(i,n,phi) - y_P_phi(t,i,n,phi) =L=
		( p_Y_max(i,n) + p_Z_max(i,n) ) * 1.1 * r_CON_y_P_max_phi(t,i,n,phi) ;

CON_y_P_max_phi_DC2(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	beta_phi(t,i,n,phi) =L= 1000 * ( 1 - r_CON_y_P_max_phi(t,i,n,phi) ) ;

* standard formulation

CON_y_P_min(t,i,n)$map_P(i,n)..							# dual to alpha
	- p_Y_min(i,n) + y_P(t,i,n) =G= 0 ;

CON_y_P_min_DC1(t,i,n)$map_P(i,n)..
	- p_Y_min(i,n) + y_P(t,i,n) =L= ( p_Y_max(i,n) + p_Z_max(i,n) ) * 1.1 * r_CON_y_P_min(t,i,n) ;

CON_y_P_min_DC2(t,i,n)$map_P(i,n)..
	alpha(t,i,n) =L= 1000 * ( 1 - r_CON_y_P_min(t,i,n) ) ;

* binary equilibrium formulation

CON_y_P_min_phi(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..			# dual to alpha_phi
	- p_Y_min(i,n) + y_P_phi(t,i,n,phi) =G= 0 ;

CON_y_P_min_phi_DC1(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	- p_Y_min(i,n) + y_P_phi(t,i,n,phi) =L= ( p_Y_max(i,n) + p_Z_max(i,n) ) * 1.1 * r_CON_y_P_min_phi(t,i,n,phi) ;

CON_y_P_min_phi_DC2(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	alpha_phi(t,i,n,phi) =L= 1000 * ( 1 - r_CON_y_P_min_phi(t,i,n,phi) ) ;

*** maximum/minimum pipeline capacity investment constraint (if active) ***

* standard formulation

CON_z_A_max(i,l)$map_A(i,l)..							# dual to eta
	a_Z_max(i,l) * x_A(i,l) - z_A(i,l) =G= 0 ;

CON_z_A_max_DC1(i,l)$map_A(i,l)..
	a_Z_max(i,l) * x_A(i,l) - z_A(i,l) =L= a_Z_max(i,l) * 1.1 * r_CON_z_A_max(i,l) ;

CON_z_A_max_DC2(i,l)$map_A(i,l)..
	eta(i,l) =L= 1000 * ( 1 - r_CON_z_A_max(i,l) ) ;

* binary equilibrium formulation

CON_z_A_max_phi(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..			# dual to eta_phi
	a_Z_max(i,l) * x_A_phi(i,l,phi) - z_A_phi(i,l,phi) =G= 0 ;

CON_z_A_max_phi_DC1(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	a_Z_max(i,l) * x_A_phi(i,l,phi) - z_A_phi(i,l,phi) =L= a_Z_max(i,l) * 1.1 * r_CON_z_A_max_phi(i,l,phi) ;

CON_z_A_max_phi_DC2(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	eta_phi(i,l,phi) =L= 1000 * ( 1 - r_CON_z_A_max_phi(i,l,phi) ) ;

* standard formulation

CON_z_A_min(i,l)$map_A(i,l)..							# dual to nu
	- a_Z_min(i,l) * x_A(i,l) + z_A(i,l) =G= 0 ;

CON_z_A_min_DC1(i,l)$map_A(i,l)..
	- a_Z_min(i,l) * x_A(i,l) + z_A(i,l) =L= a_Z_max(i,l) * 1.1 * r_CON_z_A_min(i,l) ;

CON_z_A_min_DC2(i,l)$map_A(i,l)..
	nu(i,l) =L= 1000 * ( 1 - r_CON_z_A_min(i,l) ) ;

* binary equilibrium formulation

CON_z_A_min_phi(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..			# dual to nu_phi
	- a_Z_min(i,l) * x_A_phi(i,l,phi) + z_A_phi(i,l,phi) =G= 0 ;

CON_z_A_min_phi_DC1(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	- a_Z_min(i,l) * x_A_phi(i,l,phi) + z_A_phi(i,l,phi) =L= a_Z_max(i,l) * 1.1 * r_CON_z_A_min_phi(i,l,phi) ;

CON_z_A_min_phi_DC2(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	nu_phi(i,l,phi) =L= 1000 * ( 1 - r_CON_z_A_min_phi(i,l,phi) ) ;

*** maximum pipeline capacity utilization constraint ***

*standard formulation

CON_y_A_max(t,i,l)$map_A(i,l)..							# dual to mu
	 a_Y_max(i,l) + z_A(i,l) - y_A(t,i,l) =G= 0 ;

CON_y_A_max_DC1(t,i,l)$map_A(i,l)..
	a_Y_max(i,l) + z_A(i,l) - y_A(t,i,l) =L= ( a_Y_max(i,l) + a_Z_max(i,l) ) * 1.1 * r_CON_y_A_max(t,i,l) ;

CON_y_A_max_DC2(t,i,l)$map_A(i,l)..
	mu(t,i,l) =L= 1000 * ( 1 - r_CON_y_A_max(t,i,l) ) ;

* binary equilibrium formulation

CON_y_A_max_phi(t,i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..			# dual to mu_phi
	a_Y_max(i,l) + z_A_phi(i,l,phi) - y_A_phi(t,i,l,phi) =G= 0 ;

CON_y_A_max_phi_DC1(t,i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	a_Y_max(i,l) + z_A_phi(i,l,phi) - y_A_phi(t,i,l,phi) =L=
		( a_Y_max(i,l) + a_Z_max(i,l) ) * 1.1 * r_CON_y_A_max_phi(t,i,l,phi) ;

CON_y_A_max_phi_DC2(t,i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	mu_phi(t,i,l,phi) =L= 1000 * ( 1 - r_CON_y_A_max_phi(t,i,l,phi) ) ;

* note that there is no explicit lower bound on pipeline capacity utilization
* the non-negativity constraint is included by the greater-or-equal formulation of the y_A stationarity condition

*** incentive compatibility constraint ***

* translation of short-term linear profits (without binary, lump-sum costs) into (linear) kappa variables
PROFIT_PHI(i,phi)$( phi_I(i,phi) )..
*  short-term profits in binary strategy phi (dual of objective function with fixed binary variables)
	sum(n$map_P(i,n),
	    p_Z_max(i,n) * x_P_phi(i,n,phi) * rho_phi(i,n,phi)
	  - p_Z_min(i,n) * x_P_phi(i,n,phi) * theta_phi(i,n,phi) )
	+ sum((t,n)$map_P(i,n),
	    p_Y_max(i,n) * beta_phi(t,i,n,phi)
	  - p_Y_min(i,n) * alpha_phi(t,i,n,phi) )
	+ sum(l$map_A(i,l),
	    a_Z_max(i,l) * x_A_phi(i,l,phi) * eta_phi(i,l,phi)
	  - a_Z_min(i,l) * x_A_phi(i,l,phi) * nu_phi(i,l,phi) )
	+ sum((t,l)$map_A(i,l),
	    a_Y_max(i,l) * mu_phi(t,i,l,phi) )
* binary investment costs in binary strategy phi
	- sum(n$map_P(i,n), p_X_inv(i,n) * x_P_phi(i,n,phi) )
	- sum(l$map_A(i,l), a_X_inv(i,l) * x_A_phi(i,l,phi) )
	=E=
	  kappa_act_profit(i,phi) - kappa_act_loss(i,phi)
	+ kappa_inact_profit(i,phi) - kappa_inact_loss(i,phi) ;

* ensure that actual profit (kappa_act_xxx) is greater than any other binary strategy
INCENTIVE_COMP(i,phi2)$( phi_I(i,phi2) )..
* actual profits in chosen (optimal) strategy
	sum(phi$( phi_I(i,phi) ), kappa_act_profit(i,phi) - kappa_act_loss(i,phi) ) + zeta(i) =G=
* counterfactual profits in alternative strategy phi2
	kappa_inact_profit(i,phi2) - kappa_inact_loss(i,phi2)
;

* ensure that the "correct" kappa variables are assigned
KAPPA_ACTIVE(i,phi)$( phi_I(i,phi) )..
	kappa_act_profit(i,phi) + kappa_act_loss(i,phi) =L= 100000 * x_phi(i,phi) ;

KAPPA_INACTIVE(i,phi)$( phi_I(i,phi) )..
	kappa_inact_profit(i,phi) + kappa_inact_loss(i,phi) =L= 100000 * ( 1 - x_phi(i,phi) ) ;

* constraint that exactly one strategy is choosen in equilibrium
CON_x_phi(i)..
	sum(phi$phi_I(i,phi), x_phi(i,phi) ) =E= 1 ;

*** "translation" constraints (to determine equilibrium decisions) ***

* investment in production capacity
TRANS_z_P_phi_ge(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	z_P(i,n) =g= z_P_phi(i,n,phi) - 1.1 * ( 1 - x_phi(i,phi) )  * p_Z_max(i,n) ;

TRANS_z_P_phi_le(i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	z_P(i,n) =l= z_P_phi(i,n,phi) + 1.1 * ( 1 - x_phi(i,phi) ) * p_Z_max(i,n) ;

* investment in pipeline capacity
TRANS_z_A_phi_ge(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	z_A(i,l) =g= z_A_phi(i,l,phi) - 1.1 * ( 1 - x_phi(i,phi) ) * a_Z_max(i,l) ;

TRANS_z_A_phi_le(i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	z_A(i,l) =l= z_A_phi(i,l,phi) + 1.1 * ( 1 - x_phi(i,phi) ) * a_Z_max(i,l) ;

* production quantity decision
TRANS_y_P_phi_ge(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	y_P(t,i,n) =g= y_P_phi(t,i,n,phi) - 1.1 * ( 1 - x_phi(i,phi) ) * ( p_Z_max(i,n) + p_Y_max(i,n) ) ;

TRANS_y_P_phi_le(t,i,n,phi)$( phi_I(i,phi) AND map_P(i,n) )..
	y_P(t,i,n) =l= y_P_phi(t,i,n,phi) + 1.1 * ( 1 - x_phi(i,phi) ) * ( p_Z_max(i,n) + p_Y_max(i,n) ) ;

* transport quantity decision
TRANS_y_A_phi_ge(t,i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	y_A(t,i,l) =g= y_A_phi(t,i,l,phi) - 1.1 * ( 1 - x_phi(i,phi) ) * ( a_Z_max(i,l) + a_Y_max(i,l) ) ;

TRANS_y_A_phi_le(t,i,l,phi)$( phi_I(i,phi) AND map_A(i,l) )..
	y_A(t,i,l) =l= y_A_phi(t,i,l,phi) + 1.1 * ( 1 - x_phi(i,phi) ) * ( a_Z_max(i,l) + a_Y_max(i,l) ) ;

* sales quantity decision
TRANS_y_S_phi_ge(t,i,n,phi)$( phi_I(i,phi) AND map_N(i,n) AND int_D(t,n) )..
	y_S(t,i,n) =g= y_S_phi(t,i,n,phi) - 1.1 * ( 1 - x_phi(i,phi) ) * sum(nn, p_Z_max(i,nn) + p_Y_max(i,nn) ) ;

TRANS_y_S_phi_le(t,i,n,phi)$( phi_I(i,phi) AND map_N(i,n) AND int_D(t,n) )..
	y_S(t,i,n) =l= y_S_phi(t,i,n,phi) + 1.1 * ( 1 - x_phi(i,phi) ) * sum(nn, p_Z_max(i,nn) + p_Y_max(i,nn) ) ;

************************************************************************************************************************
*** Model declaration statements                                                                                     ***
************************************************************************************************************************

*** standard welfare maximization problem ***

Model binary_optimal /
	obj_simple
	MCC
	CON_MBC
	CON_z_P_max
	CON_z_P_min
	CON_y_P_max
	CON_y_P_min
	CON_z_A_max
	CON_z_A_min
	CON_y_A_max
/ ;
binary_optimal.optcr = 0 ;

*** disjunctive-constraints formulation of standard welfare problem ***

Model disjunctive_constraints /
	obj_simple
	KKT_D
	MCC
	KKT_z_P
	KKT_y_P
	KKT_z_A
	KKT_y_A, KKT_y_A_DC1, KKT_y_A_DC2
	KKT_y_S
	CON_MBC
	CON_z_P_max, CON_z_P_max_DC1, CON_z_P_max_DC2
	CON_z_P_min, CON_z_P_min_DC1, CON_z_P_min_DC2
	CON_y_P_max, CON_y_P_max_DC1, CON_y_P_max_DC2
	CON_y_P_min, CON_y_P_min_DC1, CON_y_P_min_DC2
	CON_z_A_max, CON_z_A_max_DC1, CON_z_A_max_DC2
	CON_z_A_min, CON_z_A_min_DC1, CON_z_A_min_DC2
	CON_y_A_max, CON_y_A_max_DC1, CON_y_A_max_DC2
 / ;
disjunctive_constraints.optcr = 0 ;


*** binary equilibrium model ***

Model binary_equilibrium_Scarf /
	obj_binary_equilibrium
	KKT_D
	MCC
	KKT_z_P_phi
	KKT_y_P_phi
	KKT_z_A_phi
	KKT_y_A_phi, KKT_y_A_phi_DC1, KKT_y_A_phi_DC2
	KKT_y_S_phi_Scarf
	CON_MBC_phi
	CON_z_P_max_phi, CON_z_P_max_phi_DC1, CON_z_P_max_phi_DC2
	CON_z_P_min_phi, CON_z_P_min_phi_DC1, CON_z_P_min_phi_DC2
	CON_y_P_max_phi, CON_y_P_max_phi_DC1, CON_y_P_max_phi_DC2
	CON_y_P_min_phi, CON_y_P_min_phi_DC1, CON_y_P_min_phi_DC2
	CON_z_A_max_phi, CON_z_A_max_phi_DC1, CON_z_A_max_phi_DC2
	CON_z_A_min_phi, CON_z_A_min_phi_DC1, CON_z_A_min_phi_DC2
	CON_y_A_max_phi, CON_y_A_max_phi_DC1, CON_y_A_max_phi_DC2
	PROFIT_PHI
	INCENTIVE_COMP
	KAPPA_ACTIVE
	KAPPA_INACTIVE
	CON_x_phi
	TRANS_z_P_phi_ge
	TRANS_z_P_phi_le
	TRANS_z_A_phi_ge
	TRANS_z_A_phi_le
	TRANS_y_P_phi_ge
	TRANS_y_P_phi_le
	TRANS_y_A_phi_ge
	TRANS_y_A_phi_le
	TRANS_y_S_phi_ge
	TRANS_y_S_phi_le
 / ;
binary_equilibrium_Scarf.optcr = 0 ;

*** binary equilibrium model ***

Model binary_equilibrium_anticipation /
	obj_binary_equilibrium
	KKT_D
	MCC
	KKT_z_P_phi
	KKT_y_P_phi
	KKT_z_A_phi
	KKT_y_A_phi, KKT_y_A_phi_DC1, KKT_y_A_phi_DC2
	KKT_y_S_phi_anticipation
	CON_MBC_phi
	CON_z_P_max_phi, CON_z_P_max_phi_DC1, CON_z_P_max_phi_DC2
	CON_z_P_min_phi, CON_z_P_min_phi_DC1, CON_z_P_min_phi_DC2
	CON_y_P_max_phi, CON_y_P_max_phi_DC1, CON_y_P_max_phi_DC2
	CON_y_P_min_phi, CON_y_P_min_phi_DC1, CON_y_P_min_phi_DC2
	CON_z_A_max_phi, CON_z_A_max_phi_DC1, CON_z_A_max_phi_DC2
	CON_z_A_min_phi, CON_z_A_min_phi_DC1, CON_z_A_min_phi_DC2
	CON_y_A_max_phi, CON_y_A_max_phi_DC1, CON_y_A_max_phi_DC2
	PROFIT_PHI
	INCENTIVE_COMP
	KAPPA_ACTIVE
	KAPPA_INACTIVE
	CON_x_phi
	TRANS_z_P_phi_ge
	TRANS_z_P_phi_le
	TRANS_z_A_phi_ge
	TRANS_z_A_phi_le
	TRANS_y_P_phi_ge
	TRANS_y_P_phi_le
	TRANS_y_A_phi_ge
	TRANS_y_A_phi_le
	TRANS_y_S_phi_ge
	TRANS_y_S_phi_le
 / ;
binary_equilibrium_anticipation.optcr = 0 ;


************************************************************************************************************************
*** Model solve statements and write results to output reports                                                       ***
************************************************************************************************************************

*** standard welfare maximization problem ***
* this is the standard welfare-maximizing unit-commitment problem, with end-use prices
* taken from the duals to the market clearing constraint, and firm profits computed from those prices

Solve binary_optimal maximizing obj using MIQCP ;
$SETGLOBAL model_type "binary_optimal"
$SETGLOBAL case "'binary_opt'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),

price.l(t,n) = MCC.m(t,n) ;

$INCLUDE includes/report.gms
) ;

*** disjunctive-constraints formulation of standard welfare problem ***
* this model formulation is just for checking whether the disjunctive constraints formulation is
* correctly implemented - if this does not give identical results as the previous model, something is wrong!

Solve disjunctive_constraints maximizing obj using MIQCP ;
$SETGLOBAL model_type "disjunctive_constraints"
$SETGLOBAL case "'disj_cons'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),

$INCLUDE includes/report.gms
) ;

*** binary equilibrium/multi-objective problem: binary Nash equilibrium following Scarf's condition ***

Solve binary_equilibrium_Scarf maximizing obj using MIQCP ;
$SETGLOBAL model_type "binary_equilibrium_Scarf"
$SETGLOBAL case "'MOPBQE Scarf'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),

* re-assign actual binary decisions from the binary strategy decision variable
x_P.l(i,n) = sum(phi, x_phi.l(i,phi) * x_P_phi(i,n,phi) ) ;
x_A.l(i,l) = sum(phi, x_phi.l(i,phi) * x_A_phi(i,l,phi) ) ;

$INCLUDE includes/report.gms
$INCLUDE includes/report_binary_equilibrium.gms
) ;

*** binary equilibrium/multi-objective problem: binary Nash equilibrium (Scarf's condition) without compensation ***

zeta.fx(i) = 0 ;

Solve binary_equilibrium_Scarf maximizing obj using MIQCP ;
$SETGLOBAL model_type "binary_equilibrium_Scarf"
$SETGLOBAL case "'MOPBQE Scarf (no comp)'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),

* re-assign actual binary decisions from the binary strategy decision variable
x_P.l(i,n) = sum(phi, x_phi.l(i,phi) * x_P_phi(i,n,phi) ) ;
x_A.l(i,l) = sum(phi, x_phi.l(i,phi) * x_A_phi(i,l,phi) ) ;

$INCLUDE includes/report.gms
$INCLUDE includes/report_binary_equilibrium.gms
) ;

*** binary equilibrium/multi-objective problem: binary Nash equilibrium with anticipation of own impact ***

zeta.up(i) = inf ;

Solve binary_equilibrium_anticipation maximizing obj using MIQCP ;
$SETGLOBAL model_type "binary_equilibrium_anticipation"
$SETGLOBAL case "'MOPBQE anticipation'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),

* re-assign actual binary decisions from the binary strategy decision variable
x_P.l(i,n) = sum(phi, x_phi.l(i,phi) * x_P_phi(i,n,phi) ) ;
x_A.l(i,l) = sum(phi, x_phi.l(i,phi) * x_A_phi(i,l,phi) ) ;

$INCLUDE includes/report.gms
$INCLUDE includes/report_binary_equilibrium.gms
) ;

*** display output reports ***

display report_summary ;

execute_unload 'report_resource_market.gdx' report_summary, report_wf, report_dev ;

************************************************************************************************************************
*** The end - Have a nice day!                                                                                       ***
************************************************************************************************************************
