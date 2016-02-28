$TITLE Equilibrium problems with binary decision variables - Power market example

$ONTEXT
Daniel Huppmann, Sauleh Siddiqui
Johns Hopkins University, International Institute for Applied Systems Analysis (IIASA), DIW Berlin
huppmann (at) iiasa.ac.at, siddiqui (at) jhu.edu

Application of a multi-objective program subject to a binary quasi-equilibrium between non-cooperative players
to an example of a unit commitment problem in a nodal-pricing power market with DC-load flow representation

Manuscript published as DIW Discussion Paper 1475, 2015 (http://diw.de/sixcms/detail.php?id=diw_01.c.502763.de),
also posted on arXiv (http://arxiv.org/abs/1504.05894)
and on Optimization Online (http://www.optimization-online.org/DB_HTML/2015/04/4874.html)

Please cite as:
Daniel Huppmann and Sauleh Siddiqui.
"An exact solution method for binary equilibrium problems with compensation and the power market uplift problem",
DIW Discussion Paper 1475, 2015.

Equation numbers refer to Chapter 4 of the manuscript

Data modified from Example 5.1, in:
Steven Gabriel, Antonio Conejo, Carlos Ruiz & Sauleh Siddiqui.
"Solving discretely-constrained, Mixed Complementarity Problems with Applications in Energy",
Computers & Operations Research, 40(5):1339-1350, 2013.
DOI: http://dx.doi.org/10.1016/j.cor.2012.10.017

This work is licensed under a Creative Commons Attribution 4.0 International License
-> http://creativecommons.org/licenses/by/4.0/

For more information and applications of binary equilibrium problems, please visit:
-> https://www.github.com/danielhuppmann/binary_equilibrium

Version: February 27, 2015
$OFFTEXT

$EOLCOM #
Option MIP = GUROBI;

************************************************************************************************************************
*** Sets and parameters                                                                                              ***
************************************************************************************************************************

*** Sets ***

Sets
	n nodes		/ n1*n6 /
	i generators	/ g1*g9 /
	b gen blocks	/ b1 /
	j demand	/ d1*d4 /
	k demand blocks / k1 /
	t time		/ t1*t2 /
	l power lines	/ l1*l8 /
;

Alias (n,m) ;
Alias (i,ii) ;

*** Parameters ***

Parameters
	c_G(t,i,b)	marginal cost of generation block b
	c_on(i)		start-up costs of unit i
	c_off(i)	ramp-down costs of unit i
	x_init(i)	initial on-off status of unit i
	g_max(t,i,b)	generation capacity of block b
	g_min(t,i)	minimum generation level of unit i
	u_D(t,j,k)	marginal utility of demand block k
	d_max(t,j,k)	upper limit of demand block k
	H_M(l,n)	susceptance matrix (line-by-node)
	B_M(n,m)	susceptance matrix (node-to-node)
	f_max(l)	maximum line capacity
	slack(n) 	define slack bus (voltage angle = 0 by defintion)
;

*** Dispatch schedule set-up and mapping to time ***
* this method to automatically generate all dispatch schedule options and map them to the respective time steps (hours)
* is based on code provided by Ingmar Schlecht @ Universität Basel < ingmar.schlecht (at) unibas.ch / github: @ingmars >

$eval phi_Card 2**card(t)
Set phi /phi1*phi%phi_Card%/;    # dimension of set Phi is 2^T to describe all dispatch schedule options

Parameter
	map_phi(phi,t)	mapping of on-off decision (dispatch schedule) to time
;

map_phi(phi,t) = floor( mod( ( ord(phi) - 1 ) / ( power(2, ORD(t) - 1 ) ), 2 ) ) ;

display phi, map_phi ;

************************************************************************************************************************
*** Input data based on power market example by Gabriel et al. (2012)                                                ***
************************************************************************************************************************

*** data for generators ***

* if using multiple load blocks, costs must be non-decreasing
c_G(t,'g1','b1') = 24 ;
c_G(t,'g2','b1') = 22 ;
c_G(t,'g3','b1') = 20 ;
c_G(t,'g4','b1') = 18 ;
c_G(t,'g5','b1') = 16 ;
c_G(t,'g6','b1') = 14 ;
c_G(t,'g7','b1') = 12 ;
c_G(t,'g8','b1') = 10 ;
c_G(t,'g9','b1') = 14 ;

* make sure that the first load block can satisfy the minimum generation constraint
g_max(t,i,b) = 50 ;
g_min(t,i) = 25 ;

c_on('g1') = 100 ;
c_on('g2') = 140 ;
c_on('g3') = 180 ;
c_on('g4') = 220 ;
c_on('g5') = 250 ;
c_on('g6') = 300 ;
c_on('g7') = 350 ;
c_on('g8') = 500 ;
c_on('g9') = 105 ;

c_off('g1') = 500 ;
c_off('g2') = 350 ;
c_off('g3') = 300 ;
c_off('g4') = 250 ;
c_off('g5') = 220 ;
c_off('g6') = 180 ;
c_off('g7') = 140 ;
c_off('g8') = 100 ;
c_off('g9') = 100 ;

x_init(i) = 0 ;
x_init('g3') = 1 ;
x_init('g4') = 1 ;
x_init('g5') = 1 ;
x_init('g6') = 1 ;

*** demand data ***

u_D('t1','d1','k1') = 25 ;
u_D('t1','d2','k1') = 26 ;
u_D('t1','d3','k1') = 26 ;
u_D('t1','d4','k1') = 27 ;
u_D('t2','d1','k1') = 20 ;
u_D('t2','d2','k1') = 20 ;
u_D('t2','d3','k1') = 21 ;
u_D('t2','d4','k1') = 21 ;

d_max('t1',j,k) = 100 ;
d_max('t2',j,k) = 50 ;

*** mapping parameters (generators and demand to nodes) ***

Parameters
	map_G(n,i)	mapping of generators to nodes /
		n1.g1 1
		n1.g2 1
		n2.g3 1
		n2.g4 1
		n3.g5 1
		n3.g6 1
		n5.g7 1
		n6.g8 1
		n3.g9 1 /
	map_D(n,j)	mapping of demand to nodes /
		n3.d1	1
		n4.d2	1
		n5.d3	1
		n6.d4	1 /
;

*** network parameters ***

* power line incidence matrix
Table incidence(l,n) 	matches power lines and start-end nodes
	n1	n2	n3	n4	n5	n6
l1	1	-1
l2	1		-1
l3		1	-1
l4		1		-1
l5			1			-1
l6				1	-1
l7				1		-1
l8					1	-1
;

* DCLF matrices
H_M(l,n) = 100 * Incidence(l,n) ;
B_M(n,m) = SUM(l, Incidence(l,n) * 100 * Incidence(l,m) ) ;

* power line thermal capacities
f_max(l) = 300 ;
f_max('l4') = 20 ;
f_max('l5') = 20 ;

* phase angle at slack bus equals 0 by defintion - necessary for tractability of DCLF approach
slack('n1') = 1 ;

*** alternative market structures ***
* - game-theoretic:
* can the ISO prevent generators from entering the market (no self-scheduling) (0), or do they have
* to be compensated so that they do not have an incentive to enter the market against the dispatch
* schedule of the ISO (i.e., incentive-compatible Nash quasi-equilibrium) (1)?
* - nonnegative profits:
* does the ISO guarantee non-negative profits (yes = 1)?
* - compensation for active generators only:
* can compensation only be paid to active firms (1) or to all firms (0)?
Parameter
	game_theoretic		game-theoretic Nash equilibrium solution	/ 0 /
	nonnegativeprofits	non-negative profit rule			/ 0 /
	compensation4active	compensation only for active firms		/ 0 /
;

* the big M (which is actually an K) for the  disjunctive constraints reformulation
Parameters
* disjunctive constraints reformulation for the new-dual and compensation payments
	K_compensation 	large scalar for compensation payment constraint	/ 10000 /
* disjunctive constraints reformulation of complementarity conditions
	K_G_stat	DC scalar for KKT condiion of generator unit i		/ 10000 /
	K_G_max		DC scalar for maximum capacity constraint of unit i	/ 10000 /
	K_G_min		DC scalar for minimum activity constraint of unit i	/ 10000 /
	K_D_stat	DC scalar for KKT condition for demand of consumer j	/ 10000 /
	K_D_max		DC scalar for maximum demand constraint of consumer j	/ 10000 /
	K_F_max		DC scalar for power line capacity constraint		/ 10000 /
	K_delta		DC scalar for voltange angle band constraint		/ 10000 /
;

*** Output report parameters ***

Parameters report_iso(*,*,*,*), report_wf(*,*,*), report_dev(*,*,*), report_flow(*,*,*) ;

************************************************************************************************************************
*** Definition of variables and equations                                                                            ***
************************************************************************************************************************

*** Variables ***

Variables
	obj		objective value of upper-level objective function
	delta(t,n)	voltage angle
	price(t,n)	locational marginal price
	gamma(t)	dual to voltage angle constraint at slack node
;

Positive variables
	g(t,i,b)	generation decision of unit i
	beta(t,i,b)	dual to maximum capacity constraint for unit i
	alpha(t,i)	dual to minimum activity constraint for unit i

	g_1(t,i,b)	optimal generation of unit i if binary decision is fixed at 1
	beta_1(t,i,b)	dual of maximum capacity constraint for unit i if binary decision is fixed at 1
	alpha_1(t,i)	dual of minimum activity constraint for unit i if binary decision is fixed at 1

	d(t,j,k)	demand by consumer j
	nu_max(t,j,k)	dual to maximum demand constraint

	mu_pos(t,l)	dual to power line capacity constraint (positive direction)
	mu_neg(t,l)	dual to power line capacity constraint (negative direction)
	xi_up(t,n)	dual to upper bound for voltage angle
	xi_lo(t,n)	dual to lower bound for voltage angle

	kappa_1_plus(t,i)	absolute deviation gain (from 1 to 0)
	kappa_1_minus(t,i)	absolute deviation penalty (from 1 to 0)
	kappa_0_plus(t,i)	absolute deviation gain (from 0 to 1)
	kappa_0_minus(t,i)	absolute deviation penalty (from 0 to 1)
	zeta(i)			compensation (non-negative profits or incentive compatibility)
;

Binary variables
	x(t,i)		binary decision variable of player i (on-off decision)
* binary variables for disjunction constraints reformulation
	r_G_stat(t,i,b)	DC variable for (second etc.) generation block
	r_G_max(t,i,b)	DC variable for maximum capacity constraint of unit i
	r_G_min(t,i)	DC variable for minimum activity constraint of unit i
	r_D_stat(t,j,k)	DC variable for KKT condition for demand of consumer j
	r_D_max(t,j,k)	DC variable for maximum demand constraint of consumer j
	r_F_pos(t,l) 	DC variable for power line capacity constraint (positive direction)
	r_F_neg(t,l)	DC variable for power line capacity constraint (negative direction)
	r_delta_up(t,n)	DC variable for voltage angle band constraint (upper bound)
	r_delta_lo(t,n)	DC variable for voltage angle band constraint (lower bound)
;

Positive variables
	z_on(t,i)	ramping (start-up) decision
	z_off(t,i)	ramping (shut-down) decision
;

*** Definition of equations ***

Equations
	obj_simple	objective function of overall problem (welfare-optimal)
	obj_binary_equilibrium	objective function of overall problem (binary equilibrium problem)
* inter-temporal on-off constraints
	CON_ramp	ramp-up-down constraint
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
* incentive compatibility constraints
	INCENTIVE	incentive compatibility constraint (computing kappa by time period)
	INCENTIVE_COMP	constraint that no profitable deviation exists
	CON_POS_PROFIT	non-negative profit constraint
	CON_COMP4ACT	constraint that only active generators can receive compensation
	INC_plus	absolute deviation penalty constraint (from 1 to 0)
	INC_minus	absolute deviation penalty constraint (from 0 to 1)
* translation of optimal decision into variable "seen" by rivals
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
obj_simple..									# Equation 20 excluding compensation
	obj =e= sum((t,j,k), u_D(t,j,k) * d(t,j,k) )
	- sum((t,i,b), c_G(t,i,b) * g(t,i,b) )
	- sum((t,i), c_on(i) * z_on(t,i) + c_off(i) * z_off(t,i) ) ;
;

* objective with compensation
obj_binary_equilibrium..							# Equation 20
	obj =e= sum((t,j,k), u_D(t,j,k) * d(t,j,k) )
	- sum((t,i,b), c_G(t,i,b) * g(t,i,b) )
	- sum((t,i), c_on(i) * z_on(t,i) + c_off(i) * z_off(t,i) )
	- sum(i, zeta(i) )
;

*** inter-temporal constraint of generator dispatch - start-up and shut-down ***

CON_ramp(t,i)..									# Equation 21a
	x(t-1,i) + x_init(i)$( ORD(t) = 1 ) + z_on(t,i) - z_off(t,i) =E= x(t,i) ;

*** stationarity (KKT) conditions for demand and the voltage angle ***

KKT_D(t,j,k)..									# Equation 19a
	- u_D(t,j,k) + sum(n$map_D(n,j), price(t,n) ) + nu_max(t,j,k) =G= 0 ;

KKT_D_DC1(t,j,k)..								# Equation 19a (complementarity part 1)
	- u_D(t,j,k) + sum(n$map_D(n,j), price(t,n) ) + nu_max(t,j,k) =L= r_D_stat(t,j,k) * K_D_stat ;

KKT_D_DC2(t,j,k)..								# Equation 19a (complementarity part 2)
	d(t,j,k) =L= ( 1 - r_D_stat(t,j,k) ) * K_D_stat ;

KKT_delta(t,n)..								# Equation 19b
	SUM(m, B_M(m,n) * price(t,m) )
	+ SUM(l, H_M(l,n) * ( mu_pos(t,l) - mu_neg(t,l) ) )
	+ xi_up(t,n) - xi_lo(t,n)
	- gamma(t) * slack(n) =e= 0 ;

* set voltage angle to zero at slack node
delta.fx(t,n)$slack(n) = 0 ; 							# Equation 19i

*** market-clearing (energy balance) constraint ***

MCC(t,n)..									# Equation 19c
	sum((j,k)$( map_D(n,j) ), d(t,j,k) ) - sum((i,b)$( map_G(n,i) ), g(t,i,b) )
	+ sum(m, B_M(n,m) * delta(t,m) ) =E= 0 ;

*** maximum demand constraints  ***
CON_D_max(t,j,k)..								# Equation 19d
	d_max(t,j,k) - d(t,j,k) =G= 0 ;

CON_D_max_DC1(t,j,k)..								# Equation 19d (complementarity part 1)
	d_max(t,j,k) - d(t,j,k) =L= r_D_max(t,j,k) * K_D_max ;

CON_D_max_DC2(t,j,k)..								# Equation 19d (complementarity part 2)
	nu_max(t,j,k) =L= ( 1 - r_D_max(t,j,k) ) * K_D_max ;

*** power line capacity constraints ***

CON_F_pos(t,l)..								# Equation 19e
	f_max(l) - sum(n, H_M(l,n) * delta(t,n) ) =G= 0 ;

CON_F_pos_DC1(t,l)..								# Equation 19e (complementarity part 1)
	f_max(l) - sum(n, H_M(l,n) * delta(t,n) ) =L= r_F_pos(t,l) * K_F_max ;

CON_F_pos_DC2(t,l)..								# Equation 19e (complementarity part 2)
	mu_pos(t,l) =L= ( 1 - r_F_pos(t,l) ) * K_F_max ;

CON_F_neg(t,l)..								# Equation 19f
	f_max(l) + sum(n, H_M(l,n) * delta(t,n) ) =G= 0 ;

CON_F_neg_DC1(t,l)..								# Equation 19f (complementarity part 1)
	f_max(l) + sum(n, H_M(l,n) * delta(t,n) ) =L= r_F_neg(t,l) * K_F_max ;

CON_F_neg_DC2(t,l)..								# Equation 19f (complementarity part 2)
	mu_neg(t,l) =L= ( 1 - r_F_neg(t,l) ) * K_F_max ;

*** voltage angle band constraints ***

CON_delta_up(t,n)$( NOT slack(n) )..						# Equation 19g
	Pi - delta(t,n) =G= 0 ;

CON_delta_up_DC1(t,n)$( NOT slack(n) )..					# Equation 19g (complementarity part 1)
	Pi - delta(t,n) =L= r_delta_up(t,n) * K_delta ;

CON_delta_up_DC2(t,n)$( NOT slack(n) )..					# Equation 19g (complementarity part 2)
	xi_up(t,n) =L= ( 1 - r_delta_up(t,n) ) * K_delta ;

CON_delta_lo(t,n)$( NOT slack(n) )..						# Equation 19h
	Pi + delta(t,n) =G= 0 ;

CON_delta_lo_DC1(t,n)$( NOT slack(n) )..					# Equation 19h (complementarity part 1)
	Pi + delta(t,n) =L= r_delta_lo(t,n) * K_delta ;

CON_delta_lo_DC2(t,n)$( NOT slack(n) )..					# Equation 19h (complementarity part 2)
	xi_lo(t,n) =L= ( 1 - r_delta_lo(t,n) ) * K_delta ;

xi_up.fx(t,n)$( slack(n) ) = 0 ;
xi_lo.fx(t,n)$( slack(n) ) = 0 ;

*** first-order KKT conditions for the generators ***

* standard formulation
* for all load blocks beyond the first, equality condition 17a has to be relaxed to an inequality plus complemenarity
KKT_G(t,i,b)..									# Equation 17a
	c_G(t,i,b) - sum(n$map_G(n,i), price(t,n) ) + beta(t,i,b) - alpha(t,i) =G= 0 ;

KKT_G_DC1(t,i,b)..								# Equation 17a (complementarity part 1)
	c_G(t,i,b) - sum(n$map_G(n,i), price(t,n) ) + beta(t,i,b)
	- alpha(t,i)$( ORD(b) = 1 )
	=L= ( r_G_stat(t,i,b) * K_G_stat )$( ORD(b) > 1 ) ;

KKT_G_DC2(t,i,b)$( ORD(b) > 1 )..						# Equation 17a (complementarity part 2)
        g(t,i,b) =L= ( 1 - r_G_stat(t,i,b) ) * K_G_stat ;

* binary equilibrium formulation
* note that if the player does not switch on the plant, it cannot generate, hence g_0 = 0
* the KKT condition for x_i=0 is therefore omitted completely
KKT_G_1(t,i,b)..								# Equation 17a
	c_G(t,i,b) - sum(n$map_G(n,i), price(t,n) ) + beta_1(t,i,b) - alpha_1(t,i) =G= 0 ;

KKT_G_1_DC1(t,i,b)..								# Equation 17a (complementarity part 1)
        c_G(t,i,b) - sum(n$map_G(n,i), price(t,n) ) + beta_1(t,i,b)
	- alpha_1(t,i)$( ORD(b) = 1 )
	=L= ( r_G_stat(t,i,b) * K_G_stat )$( ORD(b) > 1 ) ;

KKT_G_1_DC2(t,i,b)$( ORD(b) > 1 )..						# Equation 17a (complementarity part 2)
        g_1(t,i,b) =L= ( 1 - r_G_stat(t,i,b) ) * K_G_stat ;

*** maximum generation capacity constraints (if active) ***

* standard formulation
CON_G_max(t,i,b)..								# Equation 17b
	x(t,i) * g_max(t,i,b) - g(t,i,b) =G= 0 ;

CON_G_max_DC1(t,i,b)..								# Equation 17b (complementarity part 1)
	x(t,i) * g_max(t,i,b) - g(t,i,b) =L= r_G_max(t,i,b) * K_G_max ;

CON_G_max_DC2(t,i,b)..								# Equation 17b (complementarity part 2)
	beta(t,i,b) =L= ( 1 - r_G_max(t,i,b) ) * K_G_max ;

* binary equilibrium formulation
CON_G_max_1(t,i,b)..								# Equation 17b
	g_max(t,i,b) - g_1(t,i,b) =G= 0 ;

CON_G_max_1_DC1(t,i,b)..							# Equation 17b (complementarity part 1)
	g_max(t,i,b) - g_1(t,i,b) =L= r_G_max(t,i,b) * K_G_max ;

CON_G_max_1_DC2(t,i,b)..							# Equation 17b (complementarity part 2)
	beta_1(t,i,b) =L= ( 1 - r_G_max(t,i,b) ) * K_G_max ;

*** minimum generation constraints (if active) ***
* standard formulation
CON_G_min(t,i)..								# Equation 17c
	- x(t,i) * g_min(t,i) + sum(b, g(t,i,b) ) =G= 0 ;

CON_G_min_DC1(t,i)..								# Equation 17c (complementarity part 1)
	- x(t,i) * g_min(t,i) + sum(b, g(t,i,b) ) =L= r_G_min(t,i) * K_G_min ;

CON_G_min_DC2(t,i)..								# Equation 17c (complementarity part 2)
	alpha(t,i) =L= ( 1 - r_G_min(t,i) ) * K_G_min ;

* binary equilibrium formulation
CON_G_min_1(t,i)..								# Equation 17c
	- g_min(t,i) + sum(b, g_1(t,i,b) ) =G= 0 ;

CON_G_min_1_DC1(t,i)..								# Equation 17c (complementarity part 1)
	- g_min(t,i) + sum(b, g_1(t,i,b) ) =L= r_G_min(t,i) * K_G_min ;

CON_G_min_1_DC2(t,i)..								# Equation 17c (complementarity part 2)
	alpha_1(t,i) =L= ( 1 - r_G_min(t,i) ) * K_G_min ;

*** incentive compatibility constraints for each player ***

INCENTIVE(t,i)..								# Equation 21a
* revenue if x_i = 1
	sum((b), beta_1(t,i,b) * g_max(t,i,b) ) - alpha_1(t,i) * g_min(t,i)
* revenue if x_i = 0 in the short run => 0
* switch value
	- kappa_1_plus(t,i) + kappa_1_minus(t,i)
	+ kappa_0_plus(t,i) - kappa_0_minus(t,i)
	=E= 0 ;

INCENTIVE_COMP(i,phi)$game_theoretic..						# Equation 21e
* actual profits less dispatch costs in (quasi-) equilibrium
	sum(t, kappa_1_plus(t,i) - kappa_1_minus(t,i) )
	- sum(t, c_on(i) * z_on(t,i) + c_off(i) * z_off(t,i) )
* compensation
	+ zeta(i)
	=G=
* profits in alternative commitment schedule z
	sum(t$( map_phi(phi,t) ),
		sum(b, beta_1(t,i,b) * g_max(t,i,b) ) - alpha_1(t,i) * g_min(t,i) )
* dispatch costs in alternative commitment schedule z
* in the mathematical write-up, the following part is written as c^D
	- sum(t$( ORD(t) > 1 AND map_phi(phi,t) > map_phi(phi,t-1) ), c_on(i) )
	- ( c_on(i) )$( x_init(i) = 0 AND map_phi(phi,'t1') = 1 )
	- sum(t$( ORD(t) > 1 AND map_phi(phi,t) < map_phi(phi,t-1) ), c_off(i) )
	- ( c_off(i) )$( x_init(i) = 1 AND map_phi(phi,'t1') = 0 )
;
* Question: should a player only have no incentive to leave the market (see next constraint),
* or should the outcome also be stable against incentives to enter the market?
* i.e., compensation against self-scheduling

CON_POS_PROFIT(i)$nonnegativeprofits..						# Equation 21e'
*profits less dispatch costs
	sum(t, kappa_1_plus(t,i) - kappa_1_minus(t,i) )
	- sum(t, c_on(i) * z_on(t,i) + c_off(i) * z_off(t,i) )
* compensation
	+ zeta(i)
	=G= 0 ;
* Question: are losses allowed?

CON_COMP4ACT(i)$compensation4active..						# Equation 21e''
	zeta(i) =L= sum(t, x(t,i) ) * K_compensation ;
* Question: Can compensation be paid to any power plant, or just to those power plants that
* actually generate electricity at some point over the model horizon?
* The parameter "compensation4active" can incorporate this regulation.

INC_plus(t,i)..									# Equation 21c
	kappa_1_plus(t,i) + kappa_1_minus(t,i)
	=L= x(t,i) * K_compensation ;

INC_minus(t,i)..								# Equation 21d
	kappa_0_plus(t,i) + kappa_0_minus(t,i)
	=L= ( 1 - x(t,i) ) * K_compensation ;

*** translating optimal strategy into equilibrium and decision seen by other players - for binary equilibrium ***
TRANS_0_ge(t,i,b)..								# Equation 21f (part 1)
	g(t,i,b) =G= 0 ;

TRANS_0_le(t,i,b)..								# Equation 21f (part 2)
	g(t,i,b) =L= x(t,i) * g_max(t,i,b) ;

TRANS_1_ge(t,i,b)..								# Equation 21g (part 1)
	g(t,i,b) =G= g_1(t,i,b) - ( 1 - x(t,i) ) * g_max(t,i,b) ;

TRANS_1_le(t,i,b)..								# Equation 21g (part 2)
	g(t,i,b) =L= g_1(t,i,b) + ( 1 - x(t,i) ) * g_max(t,i,b) ;

************************************************************************************************************************
*** Model declaration statements                                                                                     ***
************************************************************************************************************************

*** standard welfare maximization unit-commitment problem ***

Model binary_optimal /
	obj_simple
	CON_ramp
	MCC
	CON_D_max
	CON_F_pos
	CON_F_neg
	CON_delta_up
	CON_delta_lo
	CON_G_max
	CON_G_min
 / ;
binary_optimal.optcr = 0 ;

*** disjunctive-constraints formulation of standard welfare problem ***

Model disjunctive_constraints /
	obj_simple
	CON_ramp
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
disjunctive_constraints.optcr = 0 ;

*** binary equilibrium model ***

Model binary_equilibrium /
	obj_binary_equilibrium
	CON_ramp
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
	CON_G_min_1, CON_G_min_1_DC1, CON_G_min_1_DC2
	INCENTIVE
	INCENTIVE_COMP
	CON_POS_PROFIT
	CON_COMP4ACT
	INC_plus
	INC_minus
	TRANS_0_ge
	TRANS_0_le
	TRANS_1_ge
	TRANS_1_le
 / ;
binary_equilibrium.optcr = 0 ;

************************************************************************************************************************
*** Model solve statements and write results to output reports                                                       ***
************************************************************************************************************************

*** standard welfare maximization problem ***
* this is the standard welfare-maximizing unit-commitment problem, with locational marginal prices
* taken from the duals to the energy balance constraint, and generator profits computed from those prices

Solve binary_optimal maximizing obj using MIP ;

price.l(t,n) = MCC.m(t,n) ;

$SETGLOBAL case "'binary_opt'"
$INCLUDE includes/report.gms

*** disjunctive-constraints formulation of standard welfare problem ***
* this model formulation is just for checking whether the disjunctive constraints formulation is
* correctly implemented - if this does not give identical results as the previous model, something is wrong!

Solve disjunctive_constraints maximizing obj using MIP ;
$SETGLOBAL case "'disj_cons'"
$INCLUDE includes/report.gms

*** binary equilibrium/multi-objective problem: no-loss rule, and compensation to all power plants ***
* this is the "no-loss rule" case in the manuscript

nonnegativeprofits = 1 ;
compensation4active = 0 ;

Solve binary_equilibrium maximizing obj using MIP ;
$SETGLOBAL case "'BE_ZPF_ALL'"
$INCLUDE includes/report.gms
$INCLUDE includes/report_binary_equilibrium.gms

*** binary equilibrium/multi-objective problem: no-loss rule, and compensation only to active power plants ***
* this is the "no-loss & active" case in the manuscript

nonnegativeprofits = 1 ;
compensation4active = 1 ;

Solve binary_equilibrium maximizing obj using MIP ;
$SETGLOBAL case "'BE_ZPF_ACT'"
$INCLUDE includes/report.gms
$INCLUDE includes/report_binary_equilibrium.gms

*** binary equilibrium/multi-objective problem: binary Nash equilibrium (incentive-compatibility) ***
* this is the "game-theoretic" case in the manuscript

game_theoretic = 1 ;
nonnegativeprofits = 0 ;
compensation4active = 0 ;

Solve binary_equilibrium maximizing obj using MIP ;
$SETGLOBAL case "'BE_GT'"
$INCLUDE includes/report.gms
$INCLUDE includes/report_binary_equilibrium.gms

*** display output reports ***

display report_iso, report_wf, report_dev, report_flow ;

execute_unload 'report_binary_equilibrium.gdx' report_iso, report_wf, report_flow ;

************************************************************************************************************************
*** The end - Have a nice day!                                                                                       ***
************************************************************************************************************************
