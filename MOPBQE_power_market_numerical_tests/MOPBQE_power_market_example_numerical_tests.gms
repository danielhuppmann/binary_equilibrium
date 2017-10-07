$TITLE Equilibrium problems with binary decision variables - Numerical test exercise with power market example

$ONTEXT
Daniel Huppmann, Sauleh Siddiqui
Johns Hopkins University, International Institute for Applied Systems Analysis (IIASA)
huppmann (at) iiasa.ac.at, siddiqui (at) jhu.edu

Application of a multi-objective program subject to a binary quasi-equilibrium between non-cooperative players
to a large test instance of a unit commitment problem in a nodal-pricing power market with DC-load flow representation

Manuscript forthcoming in the European Journal of Operational Research (see below).
Preprints and working paper versions are available for download
on arXiv (http://arxiv.org/abs/1504.05894)
and on Optimization Online (http://www.optimization-online.org/DB_HTML/2015/04/4874.html)

Please cite as:
Daniel Huppmann and Sauleh Siddiqui.
"An exact solution method for binary equilibrium problems with compensation and the power market uplift problem",
European Journal of Operational Research, 2017, doi.org/10.1016/j.ejor.2017.09.032

Equation numbers refer to Chapters 3 and 4 of the manuscript

Data based on:
H. Pandzic, Y. Dvorkin, T. Qiu, Y. Wang, and D. Kirschen.
Unit Commitment under Uncertainty - GAMS Models
Library of the Renewable Energy Analysis Lab (REAL)
University of Washington, Seattle, USA.
Available at: http://www.ee.washington.edu/research/real/gams_code.html.

This work is licensed under a Creative Commons Attribution 4.0 International License
-> http://creativecommons.org/licenses/by/4.0/

For more information and additional examples of binary equilibrium problems, please visit:
-> http://danielhuppmann.github.io/binary_equilibrium/

$OFFTEXT

$EOLCOM #

$SETGLOBAL testcase 'region_1_2_wind_unfav'

$SETGLOBAL datasetup '*'
* mark with * to NOT convert xlsx file into include data tables
* (only needs to be done once if input data in Excel file is changed)

option MIP = GUROBI ;

file logfile / '' /;
put logfile ;

************************************************************************************************************************
*** Sets and parameters                                                                                              ***
************************************************************************************************************************

*** Sets ***

Set
	n_s	nodes (buses) 		/s101*s124,s201*s224,s301*s325/
	n (n_s)	nodes (buses) used in model instance
	days	days in sample		/d1*d7/
	t_s	time periods		/t1*t24/
	i_s	generators		/i1*i96/
	i (i_s)	generators used in model instance
	b	generator blocks	/b1*b3/
	j	generator start-up intervals /j1*j8/
	l_s	lines 			/l1*l120/
	l (l_s)	lines used in model instance
	w_s	wind power plant	/w1*w19/
	w (w_s)	wind power plant used in model instance
	column	auxiliary /column1/
	tr 	transmission data (1-HP 2-MOV 3-MS 4-YD) /tr1*tr4/
;

Alias (n,m) ;
Alias (n_s,m_s) ;
Alias (i,ii) ;
Alias (b,bb) ;

Acronym n_a ;

*** Parameters ***

Parameters
	timestep	duration (in hours) of one time step for scaling start-up costs and up-down-time constraints
	price_bound	implicit upper bound on prices (used for binding dual variables)
*	c_NL(i)		no-load cost of unit i
*	c_G(i,b)	marginal cost of unit i / generation block b
*	c_on(i)		start-up costs of unit i
	c_off(i_s)	ramp-down costs of unit i
*	x_init(i)	initial on-off status of unit i
*	z_min_up(i)	minimum uptime of unit i after start-up
*	z_min_do(i)	minimum downtime of unit i after shut-down

*	g_max(i,b)	generation capacity of unit i / block b
*	g_min(i)	minimum generation level of unit i
	u_D(t_s,n_s)	marginal utility at node n
	d_max(*,*)	upper limit at node n
*	incidence(l,n) 	matches power lines and start-end nodes
*	admittance(l) 	line admittance
	H_M(l_s,n_s)	susceptance matrix (line-by-node)
	B_M(n_s,n_s)	susceptance matrix (node-to-node)
*	f_max(l)	maximum line capacity
*	w_det(w_s,n_s)	wind feed-in
	slack(n_s) 	define slack bus (voltage angle = 0 by defintion)
;

*** load input data and case file ***

$INCLUDE input_data/input_data.gms

$INCLUDE case_data/%testcase%.gms

* create time period set
Set t (t_s) time periods used in model	/ t1*t%duration% / ;
Alias (t,tt) ;

* use all lines that connect nodes included in the network
l (l_s) = no ;
l (l_s)$( sum(n, abs( Incidence(l_s,n) ) ) eq 2 ) =  yes ;

* use all generators that are located at nodes included in the network
i (i_s) = no ;
i (i_s)$( sum(n, map_G(n,i_s) ) ) =  yes ;

* scale generator start-up costs and minimum up-down-time constraints by duration of each timestep
c_on(i) = c_on(i) / timestep ;
c_off(i) = c_off(i) / timestep ;
z_min_up(i) = z_min_up(i) / timestep ;
z_min_down(i) = z_min_down(i) / timestep ;

Parameter
	Incidence_case(l_s,n_s)
;

Incidence_case(l,n) = Incidence(l,n) ;
d_max(t,'total') = sum(n, d_max(t,n) ) ;

*** network parameters ***

* DCLF matrices
H_M(l,n) = admittance(l) * Incidence(l,n) ;
B_M(n,m) = SUM(l, Incidence(l,n) * admittance(l) * Incidence(l,m) ) ;

*** wind feed-in profile ***

* use all wind turbines that are located at nodes included in the network
w (w_s) = no ;
w (w_s)$( sum(n, map_W(w_s,n) ) ) =  yes ;

scalar w_profile_share;
w_profile_share$(wind_profile=0) =
	sum((t_s,n_s),d_max(t_s,n_s))*wind_energy_penetration/sum((t_s,w_s),w_capacity(w_s)*w_det_pu_0(t_s,w_s));
w_profile_share$(wind_profile=1) =
	sum((t_s,n_s),d_max(t_s,n_s))*wind_energy_penetration/sum((t_s,w_s),w_capacity(w_s)*w_det_pu_1(t_s,w_s));

parameter w_det(t_s,w_s);
w_det(t_s,w_s)$( wind_profile = 0 ) = w_profile_share * w_capacity(w_s) * w_det_pu_0(t_s,w_s) ;
w_det(t_s,w_s)$( wind_profile = 1 ) = w_profile_share * w_capacity(w_s) * w_det_pu_1(t_s,w_s) ;


************************************************************************************************************************
*** Load dispatch schedulle option file and assign to each generator                                                 ***
************************************************************************************************************************

* note that you have to create the dispatch option file for a specific number of time steps using the 
* file 'MOPBQE_dispatch_schedule_generation.gms'

$eval z_Card 2**%duration%
Set z /z1*z%z_Card%/;    # dimension of set Z is 2^T to describe all dispatch schedule options

Parameter
	map_Z(z,t)	mapping of on-off decision (dispatch schedule) to time
	map_Z_I(i_s,z,*) mapping of on-off decision (dispatch schedule) to time feasible for unit i
	max_Z_up(z)	minimum uptime required to dispatch option z feasible
	max_Z_down(z)	minimum downtime required to dispatch option z feasible
	init_Z_up(z)	continuous uptime following first period in model time horizon
	init_Z_down(z)	continuous downtime following first period in model time horizon
;

$if not exist 'temp_data/dispatch_option_map_%duration%.gdx' $abort !!! No dispatch schedule file for that duration !!!
execute_load 'temp_data/dispatch_option_map_%duration%.gdx', map_Z, max_Z_up, max_Z_down, init_Z_up, init_Z_down ;

* check which dispatch options are feasible for each generator

map_Z_I(i,z,'available')$( max_Z_up(z) ge z_min_up(i) and max_Z_down(z) ge z_min_down(i) ) = 1 ;
map_Z_I(i,z,t)$( map_Z_I(i,z,'available') ) = map_Z(z,t) ;

*** Output report parameters ***

Parameters report_summary(*,*,*), report_wf(*,*,*,*,*), report_iso(*,*,*,*,*), report_dev(*,*,*,*), report_flow(*,*,*,*) ;

* generator parameter report
report_wf('initial','initial',i,'initial_status','all') = x_init(i) ;
report_wf('initial','initial',i,'cap max',b) = g_max(i,b) ;
report_wf('initial','initial',i,'cap max','all') = sum(b, g_max(i,b) ) ;
report_wf('initial','initial',i,'cap min','all') = g_min(i) ;
report_wf('initial','initial',i,'noload_cost','all') = c_NL(i) ;
report_wf('initial','initial',i,'marginal_cost',b) = c_G(i,b) ;
report_wf('initial','initial',i,'suc','all') = c_on(i) ;
report_wf('initial','initial',i,'suc','pu') = c_on(i) / g_min(i) ;
report_wf('initial','initial',i,'rdc','all') = c_off(i) ;
report_wf('initial','initial',i,'rdc','pu') = c_off(i) / g_min(i) ;
report_wf('initial','initial',i,'min','up') = z_min_up(i) ;
report_wf('initial','initial',i,'min','down') = z_min_down(i) ;

report_wf('initial','initial','total','cap max','all') = sum(i, report_wf('initial','initial',i,'cap max','all') ) ;
report_wf('initial','initial','total','demand','max') = smax(t, sum(n, d_max(t,n) ) ) ;

report_wf('initial','initial',i,'dispatch options','available') = sum(z$( sum(t, map_Z_I(i,z,t) ) ), 1 ) ;
report_wf('initial','initial','total','dispatch options','max') = CARD(z) ;
report_wf('initial','initial','total','dispatch options','available') =
	sum(i, report_wf('initial','initial',i,'dispatch options','available') ) ;

************************************************************************************************************************
*** Definition of variables and equations                                                                            ***
************************************************************************************************************************

*** Variables ***

Variables
	obj		objective value of upper-level objective function
	delta(t_s,n_s)	voltage angle
	price(t_s,n_s)	locational marginal price
	gamma(t_s)	dual to voltage angle constraint at slack node
;

Positive variables
	g(t_s,i_s,b)	generation decision of unit i
	beta(t_s,i_s,b)	dual to maximum capacity constraint for unit i
	alpha(t_s,i_s)	dual to minimum activity constraint for unit i

	g_1(t_s,i_s,b)	optimal generation of unit i if binary decision is fixed at 1
	beta_1(t_s,i_s,b)	dual of maximum capacity constraint for unit i if binary decision is fixed at 1
	alpha_1(t_s,i_s)	dual of minimum activity constraint for unit i if binary decision is fixed at 1

	d(t_s,n_s)	demand at node n
	nu_max(t_s,n_s)	dual to maximum demand constraint

	w_curt(t_s,w_s)	wind curtailment

	mu_pos(t_s,l_s)	dual to power line capacity constraint (positive direction)
	mu_neg(t_s,l_s)	dual to power line capacity constraint (negative direction)
	xi_up(t_s,n_s)	dual to upper bound for voltage angle
	xi_lo(t_s,n_s)	dual to lower bound for voltage angle

	kappa_1_plus(t_s,i_s)	absolute deviation gain (from 1 to 0)
	kappa_1_minus(t_s,i_s)	absolute deviation penalty (from 1 to 0)
	kappa_0_plus(t_s,i_s)	absolute deviation gain (from 0 to 1)
	kappa_0_minus(t_s,i_s)	absolute deviation penalty (from 0 to 1)
	zeta(i_s)		compensation (non-negative profits or incentive compatibility)
;

Binary variables
	x(t_s,i_s)		binary decision variable of player i (on-off decision)
* binary variables for disjunction constraints reformulation
	r_G_stat(t_s,i_s,b)	DC variable for (second etc.) generation block
	r_G_max(t_s,i_s,b)	DC variable for maximum capacity constraint of unit i
	r_G_min(t_s,i_s)	DC variable for minimum activity constraint of unit i
	r_D_stat(t_s,n_s)	DC variable for KKT condition for demand at node n
	r_D_max(t_s,n_s)	DC variable for maximum demand constraint at node n
	r_F_pos(t_s,l_s) 	DC variable for power line capacity constraint (positive direction)
	r_F_neg(t_s,l_s)	DC variable for power line capacity constraint (negative direction)
	r_delta_up(t_s,n_s)	DC variable for voltage angle band constraint (upper bound)
	r_delta_lo(t_s,n_s)	DC variable for voltage angle band constraint (lower bound)
;

Positive variables
	z_on(t_s,i_s)	start-up decision
	z_off(t_s,i_s)	shut-down decision
;

*** Definition of equations ***

Equations
	obj_simple	objective function of overall problem (welfare-optimal)
	obj_binary_generator_profit objective function for generator profit (outer problem of welfare-optimal solution)
	obj_binary_equilibrium	objective function of overall problem (binary equilibrium problem)
* inter-temporal on-off constraints
	CON_startshut		start-up-shut-down constraint
	CON_startshut_mup	start-up-shut-down constraint for mid-load generators (minimum uptime)
	CON_startshut_mdo	start-up-shut-down constraint for mid-load generators (minimum downtime)
* stationarity conditions of ISO decision variables
	KKT_D		stationarity condition (KKT) for demand
	KKT_D_DC1, KKT_D_DC2
	KKT_delta	stationarity condition (KKT) for the voltage angle
* ISO constraints
	MCC		market-clearing constraint
	CON_D_max	maximum demand of unit j
	CON_D_max_DC1, CON_D_max_DC2
	CON_F_pos	power line capacity constraint (positive direction)
	CON_F_pos_DC1, CON_F_pos_DC2
	CON_F_neg	power line capacity constraint (negative direction)
	CON_F_neg_DC1, CON_F_neg_DC2
	CON_delta_up	constraint on voltage angle (upper bound)
	CON_delta_up_DC1, CON_delta_up_DC2
	CON_delta_lo	constraint on voltage angle (lower bound)
	CON_delta_lo_DC1, CON_delta_lo_DC2
* first-order optimality conditions of the generators
	KKT_G		stationarity condition (KKT) for generation
	KKT_G_DC1, KKT_G_DC2
	KKT_G_1		KKT condition by player if binary decision is fixed at 1
	KKT_G_1_DC1, KKT_G_1_DC2
	CON_G_max	maximum capacity constraint contingent on binary decision
	CON_G_max_DC1, CON_G_max_DC2
	CON_G_max_1	maximum capacity constraint if binary decision is fixed at 1
	CON_G_max_1_DC1, CON_G_max_1_DC2
	CON_G_min	minimum activity constraint contingent on binary decision
	CON_G_min_DC1, CON_G_min_DC2
	CON_G_min_1	minimum activity constraint if binary decision is fixed at 1
	CON_G_min_1_DC1, CON_G_min_1_DC2
*	CON_G_DC_redux, CON_G_DC_redux2
* incentive compatibility constraints
	INCENTIVE	incentive compatibility constraint (computing kappa by time period)
	INCENTIVE_COMP	constraint that no profitable deviation exists
	INC_plus	absolute deviation penalty constraint (from 1 to 0)
	INC_minus	absolute deviation penalty constraint (from 0 to 1)
* translation of optimal decision into variable in equilibrium and "seen" by rivals
	TRANS_0_ge	translation of optimal decision variable if binary decision is 0 (greater)
	TRANS_0_le	translation of optimal decision variable if binary decision is 0 (smaller)
	TRANS_1_ge	translation of optimal decision variable if binary decision is 1 (greater)
	TRANS_1_le	translation of optimal decision variable if binary decision is 1 (smaller)
;

************************************************************************************************************************
*** Declaration of equations                                                                                         ***
************************************************************************************************************************

*** Objective functions ***

* standard objective - welfare maximization less dispatch and start-up/shut-down costs
obj_simple..									# Equation 20a excluding compensation
* this is also the objective function of the inner part of Problem 15
	obj =e= sum((t,n), u_D(t,n) * d(t,n) )
	- sum((t,i), c_NL(i) * x(t,i)  )
	- sum((t,i,b), c_G(i,b) * g(t,i,b) )
	- sum((t,i), c_on(i) * z_on(t,i) + c_off(i) * z_off(t,i) ) ;
;

* objective for generator profit (given nodal prices from welfare-maximzation problem) 
obj_binary_generator_profit..					# Equation 16a
* this is the objective function of the outer constraints of Problem 15
	obj =e= sum((t,n,i,b)$( map_G(n,i) ), price.l(t,n) * g(t,i,b) )
	- sum((t,i), c_NL(i) * x(t,i)  )
	- sum((t,i,b), c_G(i,b) * g(t,i,b) )
	- sum((t,i), c_on(i) * z_on(t,i) + c_off(i) * z_off(t,i) )
;

* objective with compensation
obj_binary_equilibrium..							# Equation 20a
* this is the objective function of the multi-objective problem subject to a binary quasi-equilibrium
	obj =e= sum((t,n), u_D(t,n) * d(t,n) )
	- sum((t,i), c_NL(i) * x(t,i)  )
	- sum((t,i,b), c_G(i,b) * g(t,i,b) )
	- sum((t,i), c_on(i) * z_on(t,i) + c_off(i) * z_off(t,i) )
	- sum(i, zeta(i) )
;

*** market-clearing (energy balance) constraint ***

MCC(t,n)..									# Equation 20b
	d(t,n) - sum(w$( map_W(w,n) ), w_det(t,w) - w_curt(t,w) )
	- sum((i,b)$( map_G(n,i) ), g(t,i,b) )
	+ sum(m, B_M(n,m) * delta(t,m) ) =E= 0 ;

*** stationarity (KKT) conditions for demand and the voltage angle ***

KKT_D(t,n)..									# Equation 19a
	- u_D(t,n) + price(t,n) + nu_max(t,n) =G= 0 ;

KKT_D_DC1(t,n)..								# Equation 19a (complementarity part 1)
	- u_D(t,n) + price(t,n) + nu_max(t,n) =L= r_D_stat(t,n) * price_bound ;

KKT_D_DC2(t,n)..								# Equation 19a (complementarity part 2)
	d(t,n) =L= ( 1 - r_D_stat(t,n) ) * 1.1 * d_max(t,n) ;

KKT_delta(t,n)..								# Equation 19b
	SUM(m, B_M(m,n) * price(t,m) )
	+ SUM(l, H_M(l,n) * ( mu_pos(t,l) - mu_neg(t,l) ) )
	+ xi_up(t,n) - xi_lo(t,n)
	- gamma(t) * slack(n) =e= 0 ;

*** maximum demand constraints  ***
CON_D_max(t,n)..								# Equation 19c
	d_max(t,n) - d(t,n) =G= 0 ;

CON_D_max_DC1(t,n)..								# Equation 19c (complementarity part 1)
	d_max(t,n) - d(t,n) =L= r_D_max(t,n) * 1.1 * d_max(t,n) ;

CON_D_max_DC2(t,n)..								# Equation 19c (complementarity part 2)
	nu_max(t,n) =L= ( 1 - r_D_max(t,n) ) * price_bound ;

*** power line capacity constraints ***

CON_F_pos(t,l)..								# Equation 19d
	f_max(l) - sum(n, H_M(l,n) * delta(t,n) ) =G= 0 ;

CON_F_pos_DC1(t,l)..								# Equation 19d (complementarity part 1)
	f_max(l) - sum(n, H_M(l,n) * delta(t,n) ) =L= r_F_pos(t,l) * 2.1 * f_max(l) ;

CON_F_pos_DC2(t,l)..								# Equation 19d (complementarity part 2)
	mu_pos(t,l) =L= ( 1 - r_F_pos(t,l) ) * price_bound ;

CON_F_neg(t,l)..								# Equation 19e
	f_max(l) + sum(n, H_M(l,n) * delta(t,n) ) =G= 0 ;

CON_F_neg_DC1(t,l)..								# Equation 19e (complementarity part 1)
	f_max(l) + sum(n, H_M(l,n) * delta(t,n) ) =L= r_F_neg(t,l) * 2.1 * f_max(l) ;

CON_F_neg_DC2(t,l)..								# Equation 19e (complementarity part 2)
	mu_neg(t,l) =L= ( 1 - r_F_neg(t,l) ) * price_bound ;

*** voltage angle band constraints ***

CON_delta_up(t,n)$( NOT slack(n) )..						# Equation 19f
	Pi - delta(t,n) =G= 0 ;

CON_delta_up_DC1(t,n)$( NOT slack(n) )..					# Equation 19f (complementarity part 1)
	Pi - delta(t,n) =L= r_delta_up(t,n) * 2.1 * Pi ;

CON_delta_up_DC2(t,n)$( NOT slack(n) )..					# Equation 19f (complementarity part 2)
	xi_up(t,n) =L= ( 1 - r_delta_up(t,n) ) * price_bound ;

CON_delta_lo(t,n)$( NOT slack(n) )..						# Equation 19g
	Pi + delta(t,n) =G= 0 ;

CON_delta_lo_DC1(t,n)$( NOT slack(n) )..					# Equation 19g (complementarity part 1)
	Pi + delta(t,n) =L= r_delta_lo(t,n) * 2.1 * Pi ;

CON_delta_lo_DC2(t,n)$( NOT slack(n) )..					# Equation 19g (complementarity part 2)
	xi_lo(t,n) =L= ( 1 - r_delta_lo(t,n) ) * price_bound ;

xi_up.fx(t,n)$( slack(n) ) = 0 ;
xi_lo.fx(t,n)$( slack(n) ) = 0 ;

*** voltage angle set to zero by definition at slack node ***

delta.fx(t,n)$slack(n) = 0 ;							# Equation 19h

*** first-order KKT conditions for the generators ***

* standard formulation
* for all load blocks beyond the first, equality condition 17a has to be relaxed to an inequality plus complemenarity
KKT_G(t,i,b)..									# Equation 17a
	c_G(i,b) - sum(n$map_G(n,i), price(t,n) ) + beta(t,i,b)
	- alpha(t,i)$( ORD(b) = 1 ) =G= 0 ;

KKT_G_DC1(t,i,b)..								# Equation 17a (complementarity part 1)
	c_G(i,b) - sum(n$map_G(n,i), price(t,n) ) + beta(t,i,b)
	- alpha(t,i)$( ORD(b) = 1 )
	=L= ( r_G_stat(t,i,b) * price_bound )$( ORD(b) > 1 ) ;

KKT_G_DC2(t,i,b)$( ORD(b) > 1 )..						# Equation 17a (complementarity part 2)
        g(t,i,b) =L= ( 1 - r_G_stat(t,i,b) ) * 1.1 * g_max(i,b) ;

* binary equilibrium formulation
* note that if the player does not switch on the plant, it cannot generate, hence g_0 = 0
* the KKT condition for x_i=0 is therefore omitted completely
KKT_G_1(t,i,b)..								# Equation 17a
	c_G(i,b) - sum(n$map_G(n,i), price(t,n) ) + beta_1(t,i,b)
	- alpha_1(t,i)$( ORD(b) = 1 ) =G= 0 ;

KKT_G_1_DC1(t,i,b)..								# Equation 17a (complementarity part 1)
        c_G(i,b) - sum(n$map_G(n,i), price(t,n) ) + beta_1(t,i,b)
	- alpha_1(t,i)$( ORD(b) = 1 )
	=L= ( r_G_stat(t,i,b) * price_bound )$( ORD(b) > 1 ) ;

KKT_G_1_DC2(t,i,b)$( ORD(b) > 1 )..						# Equation 17a (complementarity part 2)
        g_1(t,i,b) =L= ( 1 - r_G_stat(t,i,b) ) * 1.1 * g_max(i,b) ;

*** maximum generation capacity constraints (if active) ***

* standard formulation
CON_G_max(t,i,b)..								# Equation 17b
	x(t,i) * g_max(i,b) - g(t,i,b) =G= 0 ;

CON_G_max_DC1(t,i,b)..								# Equation 17b (complementarity part 1)
	x(t,i) * g_max(i,b) - g(t,i,b) =L= r_G_max(t,i,b) * 1.1 * g_max(i,b) ;

CON_G_max_DC2(t,i,b)..								# Equation 17b (complementarity part 2)
	beta(t,i,b) =L= ( 1 - r_G_max(t,i,b) ) * price_bound ;

* binary equilibrium formulation
CON_G_max_1(t,i,b)..								# Equation 17b
	g_max(i,b) - g_1(t,i,b) =G= 0 ;

CON_G_max_1_DC1(t,i,b)..							# Equation 17b (complementarity part 1)
	g_max(i,b) - g_1(t,i,b) =L= r_G_max(t,i,b) * 1.1 * g_max(i,b) ;

CON_G_max_1_DC2(t,i,b)..							# Equation 17b (complementarity part 2)
	beta_1(t,i,b) =L= ( 1 - r_G_max(t,i,b) ) * price_bound ;

*** minimum generation constraints (if active) ***
* standard formulation
CON_G_min(t,i)..								# Equation 17c
	- x(t,i) * g_min(i) + g(t,i,'b1') =G= 0 ;

CON_G_min_DC1(t,i)..								# Equation 17c (complementarity part 1)
	- x(t,i) * g_min(i) + g(t,i,'b1') =L= r_G_min(t,i) * 1.1 * g_max(i,'b1') ;

CON_G_min_DC2(t,i)..								# Equation 17c (complementarity part 2)
	alpha(t,i) =L= ( 1 - r_G_min(t,i) ) * price_bound ;

* binary equilibrium formulation
CON_G_min_1(t,i)..								# Equation 17c
	- g_min(i) + g_1(t,i,'b1') =G= 0 ;

CON_G_min_1_DC1(t,i)..								# Equation 17c (complementarity part 1)
	- g_min(i) + g_1(t,i,'b1') =L= r_G_min(t,i) * 1.1 * g_max(i,'b1') ;

CON_G_min_1_DC2(t,i)..								# Equation 17c (complementarity part 2)
	alpha_1(t,i) =L= ( 1 - r_G_min(t,i) ) * price_bound ;

*** size reduction constraint for generator's optimization problem ***
* auxiliary constraint to eliminate infeasible permutations in the disjunctive-constraints formulation
* note that the minimum and the maximum generation capacity cannot both be binding at the same time
*CON_G_DC_redux(t,i)..
*        r_G_min(t,i) + r_G_max(t,i,'b1') =G= 1 ;

* note that a constraint can only be binding if a lower generation block is also binding
*CON_G_DC_redux2(t,i,b,bb)$( ORD(bb) < ORD(b) )..
*        r_G_max(t,i,bb) =L= r_G_max(t,i,b) ;

*** inter-temporal constraint of generator dispatch - start-up and shut-down ***

CON_startshut(t,i)..								# Equation 21a
	x(t-1,i) + x_init(i)$( ORD(t) = 1 ) + z_on(t,i) - z_off(t,i) =E= x(t,i) ;

CON_startshut_mup(t,i)..							# additional constraint to Problem 16
	1 - z_on(t,i) =G= sum(tt$( ORD(tt) > ORD(t)
		AND ORD(tt) < ( ORD(t) + z_min_up(i) ) ) , z_off(tt,i) ) ;

CON_startshut_mdo(t,i)..							# additional constraint to Problem 16
	1 - z_off(t,i) =G= sum(tt$( ORD(tt) > ORD(t)
		AND ORD(tt) < ( ORD(t) + z_min_down(i) ) ) , z_on(tt,i) ) ;

*** incentive compatibility constraints for each player ***

INCENTIVE(t,i)..								# Equation 21b
* revenue if x_i = 1
	sum((b), beta_1(t,i,b) * g_max(i,b) ) - alpha_1(t,i) * g_min(i)
* revenue if x_i = 0 in the short run => 0
* switch value
	- kappa_1_plus(t,i) + kappa_1_minus(t,i)
	+ kappa_0_plus(t,i) - kappa_0_minus(t,i)
	=E= 0 ;

INC_plus(t,i)..									# Equation 21c
	kappa_1_plus(t,i) + kappa_1_minus(t,i)
	=L= x(t,i) * 1.1 * price_bound * sum(b, g_max(i,b) ) ;

INC_minus(t,i)..								# Equation 21d
	kappa_0_plus(t,i) + kappa_0_minus(t,i)
	=L= ( 1 - x(t,i) ) * 1.1 * price_bound * sum(b, g_max(i,b) ) ;

INCENTIVE_COMP(i,z)$( map_Z_I(i,z,'available') )..				# Equation 21e
* actual profits less no-load and dispatch costs in (quasi-) equilibrium
	sum(t, kappa_1_plus(t,i) - kappa_1_minus(t,i) )
	- sum(t, x.l(t,i) * c_NL(i) )
	- sum(t, c_on(i) * z_on(t,i) + c_off(i) * z_off(t,i) )
* compensation
	+ zeta(i)
	=G=
* profits in alternative commitment schedule z
	sum(t$( map_Z_I(i,z,t) ),
		sum(b, beta_1(t,i,b) * g_max(i,b) ) - alpha_1(t,i) * g_min(i) )
* dispatch costs in alternative commitment schedule z
* in the mathematical write-up, the following part is written as c^D
	- sum(t$map_Z_I(i,z,t), c_NL(i) )
	- sum(t$( ORD(t) > 1  AND map_Z_I(i,z,t) > map_Z_I(i,z,t-1) ), c_on(i) )
	- ( c_on(i) )$( x_init(i) = 0 AND map_Z_I(i,z,'t1') = 1 )
	- sum(t$( ORD(t) > 1 AND map_Z_I(i,z,t) < map_Z_I(i,z,t-1) ), c_off(i) )
	- ( c_off(i) )$( x_init(i) = 1 AND map_Z_I(i,z,'t1') = 0 )
;

*** translating optimal strategy into decision seen by other players - for binary equilibrium ***
TRANS_0_ge(t,i,b)..								# Equation 21f (part 1)
	g(t,i,b) =G= 0 ;

TRANS_0_le(t,i,b)..								# Equation 21f (part 2)
	g(t,i,b) =L= x(t,i) * g_max(i,b) ;

TRANS_1_ge(t,i,b)..								# Equation 21g (part 1)
	g(t,i,b) =G= g_1(t,i,b) - ( 1 - x(t,i) ) * g_max(i,b) ;

TRANS_1_le(t,i,b)..								# Equation 21g (part 2)
	g(t,i,b) =L= g_1(t,i,b) + ( 1 - x(t,i) ) * g_max(i,b) ;

************************************************************************************************************************
*** Model declaration statements                                                                                     ***
************************************************************************************************************************

*** standard welfare maximization problem ***

Model binary_optimal /
	obj_simple
	CON_startshut
	CON_startshut_mup
	CON_startshut_mdo
	MCC
	CON_D_max
	CON_F_pos
	CON_F_neg
	CON_delta_up
	CON_delta_lo
	CON_G_max
	CON_G_min
 / ;
binary_optimal.optcr = 0.0001 ;
binary_optimal.optfile = 1 ;

*** generator profit optimization problem (given nodal prices from welfare-maximzation problem) ***

Model binary_generator_profit /
	obj_binary_generator_profit
	CON_startshut
	CON_startshut_mup
	CON_startshut_mdo
	CON_G_max
	CON_G_min
 / ;
binary_generator_profit.optcr = 0.0001 ;
binary_generator_profit.optfile = 1 ;

*** disjunctive-constraints formulation of standard welfare problem ***

Model disjunctive_constraints /
	obj_simple
	CON_startshut
	CON_startshut_mup
	CON_startshut_mdo
	KKT_D, KKT_D_DC1, KKT_D_DC2
	KKT_delta
	MCC
	CON_D_max, CON_D_max_DC1, CON_D_max_DC2
	CON_F_pos, CON_F_pos_DC1, CON_F_pos_DC2
	CON_F_neg, CON_F_neg_DC1, CON_F_neg_DC2
	CON_delta_up, CON_delta_up_DC1, CON_delta_up_DC2
	CON_delta_lo, CON_delta_lo_DC1, CON_delta_lo_DC2
	KKT_G, KKT_G_DC1, KKT_G_DC2
	CON_G_max, CON_G_max_DC1, CON_G_max_DC2
	CON_G_min, CON_G_min_DC1, CON_G_min_DC2
 / ;
disjunctive_constraints.optcr = 0.0001 ;
disjunctive_constraints.optfile = 1 ;

*** binary equilibrium model ***

Model binary_equilibrium /
	obj_binary_equilibrium
	CON_startshut
	CON_startshut_mup
	CON_startshut_mdo
	KKT_D, KKT_D_DC1, KKT_D_DC2
	KKT_delta
	MCC
	CON_D_max, CON_D_max_DC1, CON_D_max_DC2
	CON_F_pos, CON_F_pos_DC1, CON_F_pos_DC2
	CON_F_neg, CON_F_neg_DC1, CON_F_neg_DC2
	CON_delta_up, CON_delta_up_DC1, CON_delta_up_DC2
	CON_delta_lo, CON_delta_lo_DC1, CON_delta_lo_DC2
	KKT_G_1, KKT_G_1_DC1, KKT_G_1_DC2
	CON_G_max_1, CON_G_max_1_DC1, CON_G_max_1_DC2
	CON_G_min_1,
	CON_G_min_1_DC1,
	CON_G_min_1_DC2
	INCENTIVE
	INCENTIVE_COMP
	INC_plus
	INC_minus
	TRANS_0_ge
	TRANS_0_le
	TRANS_1_ge
	TRANS_1_le
/ ;
binary_equilibrium.optcr = 0.01 ;
binary_equilibrium.optfile = 1 ;

************************************************************************************************************************
*** Model solve statements and write results to output reports                                                       ***
************************************************************************************************************************

loop(days,

* assign hourly demnd for day in iteration
d_max('t1',n) = d_max_full(days,'t2',n) ;
d_max('t2',n) = d_max_full(days,'t4',n) ;
d_max('t3',n) = d_max_full(days,'t6',n) ;
d_max('t4',n) = d_max_full(days,'t8',n) ;
d_max('t5',n) = d_max_full(days,'t10',n) ;
d_max('t6',n) = d_max_full(days,'t12',n) ;
d_max('t7',n) = d_max_full(days,'t14',n) ;
d_max('t8',n) = d_max_full(days,'t16',n) ;
d_max('t9',n) = d_max_full(days,'t18',n) ;
d_max('t10',n) = d_max_full(days,'t20',n) ;
d_max('t11',n) = d_max_full(days,'t22',n) ;
d_max('t12',n) = d_max_full(days,'t24',n) ;

$SETGLOBAL scenario "days"

*** standard welfare maximization problem ***
* this is the standard welfare-maximizing unit-commitment problem, with locational marginal prices
* taken from the duals to the energy balance constraint, and generator profits computed from those prices
* this is the inner optimization problem of Problem 15

put_utility 'log' /'+++ Standard unit commitment model (social-welfare maximization) +++ ' ;

Solve binary_optimal maximizing obj using MIP ;

*** welfare-optimal results with compensation following no-loss rule (non-negative profits for all firms) ***
$SETGLOBAL model_type "binary_optimal"
$SETGLOBAL model_case "'binary_opt_noloss'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),
* assign prices from nodal energy balances duals
price.l(t,n) = MCC.m(t,n) ;

$INCLUDE includes/report.gms

* required compensation for no-loss rule
report_wf(%scenario%,%model_case%,i,'compensation','all')$( report_wf(%scenario%,%model_case%,i,'profit','all') < 0 ) =
	- report_wf(%scenario%,%model_case%,i,'profit','all') ;

* assigning compensation payments and net welfare to summary report
report_wf(%scenario%,%model_case%,'all','compensation','all') = sum(i, report_wf(%scenario%,%model_case%,i,'compensation','all') ) ;
report_wf(%scenario%,%model_case%,'all','welfare_net','all') =
	report_wf(%scenario%,%model_case%,'all','welfare','all') - report_wf(%scenario%,%model_case%,'all','compensation','all') ;

report_summary(%scenario%,%model_case%,'compensation') = sum(i, report_wf(%scenario%,%model_case%,i,'compensation','all') ) / 1e6  ;
report_summary(%scenario%,%model_case%,'welfare_net') = report_wf(%scenario%,%model_case%,'all','welfare_net','all') / 1e6 ;
) ;

* write model statistics summary
$INCLUDE includes/report_model_stats.gms

execute_unload 'temp_data/binary_optimal.gdx' ;

*** welfare-optimal results with incentive-compatilble compensation (no profitable deviation for any firm) ***
* this is the outer optimization problem of Problem 15 (welfare-optimal dispatch fixed form inner optimization problem)

* duplicate output of results from welfare-optimal solution to the inner optimization problem
$SETGLOBAL model_case "'binary_opt_incentive'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),

$INCLUDE includes/report.gms

Solve binary_generator_profit maximizing obj using MIP;
$SETGLOBAL model_type "binary_generator_profit"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),

* required compensation for no-loss rule
report_wf(%scenario%,%model_case%,i,'compensation','all') =
* highest profit possible
	sum((t,n,b)$( map_G(n,i) ), price.l(t,n) * g.l(t,i,b) )
	- sum(t, c_NL(i) * x.l(t,i) )
	- sum((t,b), c_G(i,b) * g.l(t,i,b) )
	- sum(t, c_on(i) * z_on.l(t,i) + c_off(i) * z_off.l(t,i) )
* actual profit in welfare-optimal outcome
	- report_wf(%scenario%,%model_case%,i,'profit','all') ;

* assigning compensation payments and net welfare to summary report
report_wf(%scenario%,%model_case%,'all','compensation','all') =
	sum(i, report_wf(%scenario%,%model_case%,i,'compensation','all') ) ;
report_wf(%scenario%,%model_case%,'all','welfare_net','all') =
	report_wf(%scenario%,%model_case%,'all','welfare','all')
	- report_wf(%scenario%,%model_case%,'all','compensation','all') ;

report_summary(%scenario%,%model_case%,'compensation') =
	sum(i, report_wf(%scenario%,%model_case%,i,'compensation','all') ) / 1e6  ;
report_summary(%scenario%,%model_case%,'welfare_net') =
	report_wf(%scenario%,%model_case%,'all','welfare_net','all') / 1e6 ;
) ;

* write model statistics summary
$INCLUDE includes/report_model_stats.gms

) ;

* revert to solution of social-welfare problem
execute_loadpoint 'temp_data/binary_optimal.gdx' ;

*** disjunctive-constraints formulation of standard welfare problem ***
* this model formulation is just for checking whether the disjunctive constraints formulation is
* correctly implemented - if this does not give identical results as the previous model, something is wrong!

put_utility 'log' /'+++ Standard unit commitment model (DC formulation) +++ ' ;

* set DC binary variables to 1 if constraints are not binding
$INCLUDE includes/binding_constraints.gms

* solve model with fixed binary variables (to check for problems)
x.fx(t,i) = x.l(t,i) ;
r_G_stat.fx(t,i,b)$( ORD(b) > 1 AND x.l(t,i) ) = r_G_stat.l(t,i,b) ;
r_G_stat.fx(t,i,b)$( ORD(b) > 1 AND NOT x.l(t,i) ) = 1 ;
r_G_max.fx(t,i,b) = r_G_max.l(t,i,b) ;
r_G_min.fx(t,i) = r_G_min.l(t,i) ;
r_F_pos.fx(t,l) = r_F_pos.l(t,l) ;
r_F_neg.fx(t,l) = r_F_neg.l(t,l) ;
r_delta_up.fx(t,n) = r_delta_up.l(t,n) ;
r_delta_lo.fx(t,n) = r_delta_lo.l(t,n) ;

Solve disjunctive_constraints maximizing obj using MIP ;
$SETGLOBAL model_type "disjunctive_constraints"
$SETGLOBAL model_case "'disj_cons'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),
$INCLUDE includes/report.gms

* required compensation for no-loss rule
report_wf(%scenario%,%model_case%,i,'compensation','all')$( report_wf(%scenario%,%model_case%,i,'profit','all') < 0 ) =
	- report_wf(%scenario%,%model_case%,i,'profit','all') ;

* assigning compensation payments and net welfare to summary report
report_wf(%scenario%,%model_case%,'all','compensation','all') =
	sum(i, report_wf(%scenario%,%model_case%,i,'compensation','all') ) ;
report_wf(%scenario%,%model_case%,'all','welfare_net','all') =
	report_wf(%scenario%,%model_case%,'all','welfare','all')
	- report_wf(%scenario%,%model_case%,'all','compensation','all') ;

report_summary(%scenario%,%model_case%,'compensation') =
	sum(i, report_wf(%scenario%,%model_case%,i,'compensation','all') ) / 1e6  ;
report_summary(%scenario%,%model_case%,'welfare_net') =
	report_wf(%scenario%,%model_case%,'all','welfare_net','all') / 1e6 ;
) ;

* write model statistics summary
$INCLUDE includes/report_model_stats.gms

* unfix disjunctive constraints binary variables
$INCLUDE includes/unfixing_binaries.gms

*** binary equilibrium/multi-objective problem (incentive-compatibility) - get feasible starting point ***
* this is a reduced version to obtain a good starting point for the model

put_utility 'log' /'+++ Binary equilibrium model (DC-binary starting point run) +++ ' ;

* set DC binary variables to 1 if constraints are not binding
$INCLUDE includes/binding_constraints.gms

* set initial values for binary variables for binary equilibrium formulation
r_G_stat.l(t,i,b)$( ORD(b) > 1 AND NOT x.l(t,i) ) =
	1$( sum(n$map_G(n,i), price.l(t,n) le c_G(i,b) ) ) ;
r_G_max.l(t,i,b)$( NOT x.l(t,i) ) =  1 ;
r_G_max.l(t,i,b)$( NOT x.l(t,i) AND sum(n$map_G(n,i), price.l(t,n) > c_G(i,b) ) ) = 0 ;

* solve model with fixed binary variables except for non-dispatched plants to get a good starting value
x.fx(t,i) = x.l(t,i) ;
r_G_stat.fx(t,i,b)$( x.l(t,i) AND ORD(b) > 1 ) = r_G_stat.l(t,i,b) ;
r_G_max.fx(t,i,b)$( x.l(t,i) ) = r_G_max.l(t,i,b) ;
r_G_min.fx(t,i)$( x.l(t,i) ) = r_G_min.l(t,i) ;
r_F_pos.fx(t,l) = r_F_pos.l(t,l) ;
r_F_neg.fx(t,l) = r_F_neg.l(t,l) ;
r_delta_up.fx(t,n) = r_delta_up.l(t,n) ;
r_delta_lo.fx(t,n) = r_delta_lo.l(t,n) ;

Solve binary_equilibrium maximizing obj using MIP ;

* unfix disjunctive constraints binary variables
$INCLUDE includes/unfixing_binaries.gms

*** binary equilibrium/multi-objective problem (incentive-compatibility) - run from social-welfare starting point ***
* this is the "game-theoretic" case (Problem 14,14' in the theoretical formulation, Problem 20 in Chapter 4)

put_utility 'log' /'+++ Binary equilibrium model (from starting point) +++ ' ;

Solve binary_equilibrium maximizing obj using MIP ;
$SETGLOBAL model_type "binary_equilibrium"
$SETGLOBAL model_case "'binary_eq (init)'"

if( ( %model_type%.modelstat = 1 OR %model_type%.modelstat = 8 ),
$INCLUDE includes/report.gms
$INCLUDE includes/report_binary_equilibrium.gms

* assigning compensation payments and net welfare to summary report
report_wf(%scenario%,%model_case%,'all','welfare_net','all') =
	report_wf(%scenario%,%model_case%,'all','welfare','all') - report_wf(%scenario%,%model_case%,'all','compensation','all') ;

report_summary(%scenario%,%model_case%,'welfare_net') = report_wf(%scenario%,%model_case%,'all','welfare_net','all') / 1e6 ;
) ;

* write model statistics summary
$INCLUDE includes/report_model_stats.gms

$ONTEXT

*** binary equilibrium/multi-objective problem (incentive-compatibility) - run from cleared starting point ***
* this is the "game-theoretic" case in the manuscript(again)

put_utility 'log' /'+++ Binary equilibrium model (from cleared initialization) +++ ' ;

option clear = x ;
option clear = r_G_stat ;
option clear = r_G_max ;
option clear = r_G_min ;
option clear = r_D_stat ;
option clear = r_D_max ;
option clear = r_F_pos ;
option clear = r_F_neg ;
option clear = r_delta_up ;
option clear = r_delta_lo ;

Solve binary_equilibrium maximizing obj using MIP ;
$SETGLOBAL model_case "'binary_eq (clear)'"

$INCLUDE includes/report.gms
$INCLUDE includes/report_binary_equilibrium.gms

* assigning compensation payments and net welfare to summary report
report_wf(%scenario%,%model_case%,'all','welfare_net','all') =
	report_wf(%scenario%,%model_case%,'all','welfare','all') - report_wf(%scenario%,%model_case%,'all','compensation','all') ;

report_summary(%scenario%,%model_case%,'welfare_net') = report_wf(%scenario%,%model_case%,'all','welfare_net','all') / 1e6 ;

* write model statistics summary
$INCLUDE includes/report_model_stats.gms
$OFFTEXT

*** display summary output report and write to all reports to a gdx file ***

display report_summary ;

execute_unload 'output/report_MOPBQE_%testcase%.gdx' report_summary, report_wf, report_dev, report_iso, report_flow, map_Z_I ;

) ;

************************************************************************************************************************
*** The end - Have a nice day!                                                                                       ***
************************************************************************************************************************
