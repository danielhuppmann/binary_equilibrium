$ONTEXT
GAMS code based on the file "Input_Data" by:

H. Pandzic, Y. Dvorkin, T. Qiu, Y. Wang, and D. Kirschen.
Unit Commitment under Uncertainty - GAMS Models
Library of the Renewable Energy Analysis Lab (REAL)
University of Washington, Seattle, USA.
Available at: http://www.ee.washington.edu/research/real/gams_code.html.

Downloaded Dec 10, 2015, by DH
$OFFTEXT


***************************************************************
*** OPTIONS
***************************************************************

scalar transmission_option/4/;
* 1 - HP
* 2 - MOV
* 3 - MS
* 4 - YD

***************************************************************
*** PARAMETERS
***************************************************************

*** GENERATOR DATA ***

***
table gen_map(i_s,n_s) mapping of generators to nodes
%datasetup%$call =xls2gms r=g_map!a1:bv97 i=input_data/input_UC_dh.xlsx o=temp_data/gmap.inc
$include temp_data/gmap.inc
;
Parameter map_G(n_s,i_s) ;
map_G(n_s,i_s) = gen_map(i_s,n_s) ;

***
table g_max(i_s,b) generation capacity of unit i - block b
%datasetup%$call =xls2gms r=generators_c!be2:bh98 i=input_data/input_UC_dh.xlsx o=temp_data/block_max.inc
$include temp_data/block_max.inc
;

***
table aux9(i_s,tr)
%datasetup%$call =xls2gms r=generators_c!an2:ar98 i=input_data/input_UC_dh.xlsx o=temp_data/aux9.inc
$include temp_data/aux9.inc
;
parameter g_min(i_s) minimum generation level of unit i ;
g_min(i_s)=sum(tr$(ord(tr)=transmission_option),aux9(i_s,tr));

table c_G_option(i_s,b,tr) marginal cost of unit i and generation block b
%datasetup%$call =xls2gms r=generators_c!aw2:bc290 i=input_data/input_UC_dh.xlsx o=temp_data/c_G.inc
$include temp_data/c_G.inc
;
parameter c_G(i_s,b);
c_G(i_s,b)=sum(tr$(ord(tr)=transmission_option),c_G_option(i_s,b,tr));

***
table suc_sw_option(i_s,j,tr) start-up costs of unit i
%datasetup%$call =xls2gms r=generators_c!bj2:bp770 i=input_data/input_UC_dh.xlsx o=temp_data/start_up_sw.inc
$include temp_data/start_up_sw.inc
;
parameter c_on(i_s);
c_on(i_s)=sum((j,tr)$(ord(tr)=transmission_option),suc_sw_option(i_s,j,tr));

***
table aux3(i_s,column)
%datasetup%$call =xls2gms r=generators_c!g2:h98 i=input_data/input_UC_dh.xlsx o=temp_data/aux3.inc
$include temp_data/aux3.inc
;
parameter
	x_init(i_s)	initial on-off status of unit i
	x_init_h(i_s)	hours of operation at start of model horizon ;

x_init(i_s)$( sum(column, aux3(i_s,column) ) > 0 ) = 1 ;
x_init_h(i_s) = sum(column, aux3(i_s,column) ) ;

***
table aux7(i_s,tr)
%datasetup%$call =xls2gms r=generators_c!ab2:af98 i=input_data/input_UC_dh.xlsx o=temp_data/aux7.inc
$include temp_data/aux7.inc
;
parameter z_min_down(i_s) minimum downtime of unit i after shut-down ;
z_min_down(i_s)=sum(tr$(ord(tr)=transmission_option),aux7(i_s,tr));

***
table aux8(i_s,tr)
%datasetup%$call =xls2gms r=generators_c!ah2:al98 i=input_data/input_UC_dh.xlsx o=temp_data/aux8.inc
$include temp_data/aux8.inc
;
parameter z_min_up(i_s) minimum uptime of unit i after start-up ;
z_min_up(i_s)=sum(tr$(ord(tr)=transmission_option),aux8(i_s,tr));

***
table aux4(i_s,tr)
%datasetup%$call =xls2gms r=generators_c!j2:N98 i=input_data/input_UC_dh.xlsx o=temp_data/aux4.inc
$include temp_data/aux4.inc
;
parameter c_NL(i_s) no-load cost of unit i ;
c_NL(i_s)=sum(tr$(ord(tr)=transmission_option),aux4(i_s,tr));

table aux5(i_s,tr)
%datasetup%$call =xls2gms r=generators_c!p2:t98 i=input_data/input_UC_dh.xlsx o=temp_data/aux5.inc
$include temp_data/aux5.inc
;
parameter ramp_up(i_s) generator ramp-up limit;
ramp_up(i_s)=sum(tr$(ord(tr)=transmission_option),aux5(i_s,tr));

table aux6(i_s,tr)
%datasetup%$call =xls2gms r=generators_c!v2:z98 i=input_data/input_UC_dh.xlsx o=temp_data/aux6.inc
$include temp_data/aux6.inc
;
parameter ramp_down(i_s) generator ramp-down limit;
ramp_down(i_s)=sum(tr$(ord(tr)=transmission_option),aux6(i_s,tr));

*** DEMAND DATA ***

table d_max_full(days,t_s,n_s) yearly demand at node n
*$call =xls2gms r=load!k2:cf38 i=input_UC_dh.xlsx o=load.inc
%datasetup%$call =xls2gms r=load!j2:cf170 i=input_data/input_UC_dh.xlsx o=temp_data/load24.inc
$include temp_data/load24.inc
;

*** LINE DATA ***

table aux11(l_s,column)
%datasetup%$call =xls2gms r=line_admittance!a2:b122 i=input_data/input_UC_dh.xlsx o=temp_data/aux11.inc
$include temp_data/aux11.inc
;
parameter admittance(l_s) line admittance;
admittance(l_s)=sum(column,aux11(l_s,column));

***
table incidence(l_s,n_s) 	matches power lines and start-end nodes
%datasetup%$call =xls2gms r=line_map!a1:bv121 i=input_data/input_UC_dh.xlsx o=temp_data/line_map.inc
$include temp_data/line_map.inc
;

***
table aux12(l_s,column)
%datasetup%$call =xls2gms r=line_admittance!d2:e122 i=input_data/input_UC_dh.xlsx o=temp_data/aux12.inc
$include temp_data/aux12.inc
;
parameter f_max(l_s)	maximum line capacity ;
f_max(l_s)=sum(column,aux12(l_s,column));

*** WIND DATA ***

table map_W(w_s,n_s) wind power plant map
%datasetup%$call =xls2gms r=wind_map!a1:bv20 i=input_data/input_UC_dh.xlsx o=temp_data/w_map.inc
$include temp_data/w_map.inc
;

table aux21(w_s,column) wind power plant capacities
%datasetup%$call =xls2gms r=wind_test!aa4:ab23 i=input_data/input_UC_dh.xlsx o=temp_data/w_capacities.inc
$include temp_data/w_capacities.inc
;
parameter w_capacity(w_s) line plant capacity;
w_capacity(w_s)=sum(column,aux21(w_s,column));


table w_det_pu_1(t_s,w_s) available wind generation
%datasetup%$call =xls2gms r=wind_data!a5:t29 i=input_data/input_UC_dh.xlsx o=temp_data/w_det1.inc
$include temp_data/w_det1.inc
;
table w_det_pu_0(t_s,w_s) available wind generation
%datasetup%$call =xls2gms r=wind_data!w5:ap29 i=input_data/input_UC_dh.xlsx o=temp_data/w_det0.inc
$include temp_data/w_det0.inc
;

$ONTEXT

* MORE GENERATOR DATA

table suc_sl(i,u) generator stepwise start-up hourly blocks
$call =xls2gms r=generators_c!br2:bz98 i=input_data/input_UC_dh.xlsx o=start_up_sl.inc
$include start_up_sl.inc
;

table aux2(i,column)
$call =xls2gms r=generators_c!d2:e98 i=input_data/input_UC_dh.xlsx o=aux2.inc
$include aux2.inc
;
parameter count_off_init(i) number of time periods each generator has been off;
count_off_init(i)=sum(column,aux2(i,column));

table aux3(i,column)
$call =xls2gms r=generators_c!g2:h98 i=input_data/input_UC_dh.xlsx o=aux3.inc
$include aux3.inc
;
parameter count_on_init(i) number of time periods each generator has been on;
count_on_init(i)=sum(column,aux3(i,column));

table aux10(i,column)
$call =xls2gms r=generators_c!at2:au98 i=input_data/input_UC_dh.xlsx o=aux10.inc
$include aux10.inc
;
parameter g_0(i) generator generation at t=0;
g_0(i)=sum(column,aux10(i,column));

parameter onoff_t0(i) on-off status at t=0;
onoff_t0(i)$(count_on_init(i) gt 0) = 1;

parameter L_up_min(i) used for minimum up time constraints;
L_up_min(i) = min(card(t), (g_up(i)-count_on_init(i))*onoff_t0(i));

parameter L_down_min(i) used for minimum up time constraints;
L_down_min(i) = min(card(t), (g_down(i)-count_off_init(i))*(1-onoff_t0(i)));

scalar M number of hours a unit can be on or off /2600/;

* MORE WIND DATA

table wind_robust_pu_1(t,w,robust)
$call =xls2gms r=wind_data!a35:e491 i=input_UC_dh.xlsx o=w_robust_1.inc
$include w_robust_1.inc
;
table wind_robust_pu_0(t,w,robust)
$call =xls2gms r=wind_data!w35:aa491 i=input_UC_dh.xlsx o=w_robust_0.inc
$include w_robust_0.inc
;
parameter wind_robust(t,w,robust);
wind_robust(t,w,robust)$(wind_profile=0)=mnozitelj*w_capacity(w)*wind_robust_pu_0(t,w,robust);
wind_robust(t,w,robust)$(wind_profile=1)=mnozitelj*w_capacity(w)*wind_robust_pu_1(t,w,robust);


table wind_stoch_max_1(t,w,robust)
$call =xls2gms r=wind_data!h35:l491 i=input_UC_dh.xlsx o=w_stoch_1.inc
$include w_stoch_1.inc
;
table wind_stoch_max_0(t,w,robust)
$call =xls2gms r=wind_data!ad35:ah491 i=input_UC_dh.xlsx o=w_stoch_0.inc
$include w_stoch_0.inc
;
parameter wind_stoch_max(t,w,robust);
wind_stoch_max(t,w,robust)$(wind_profile=0)=mnozitelj*w_capacity(w)*wind_stoch_max_0(t,w,robust);
wind_stoch_max(t,w,robust)$(wind_profile=1)=mnozitelj*w_capacity(w)*wind_stoch_max_1(t,w,robust);


table prob_aux(scen,column)
$call =xls2gms r=wind_test!ad4:ae14 i=input_UC_dh.xlsx o=prob_aux.inc
$include prob_aux.inc
;
parameter prob(scen) stochastic scenario probabilities;
prob(scen)=sum(column,prob_aux(scen,column));

table wind_scenarios_1(t,w,scen)
$call =xls2gms r=wind_data!a496:m952 i=input_UC_dh.xlsx o=w_scen_1.inc
$include w_scen_1.inc
;
table wind_scenarios_0(t,w,scen)
$call =xls2gms r=wind_data!w496:ai952 i=input_UC_dh.xlsx o=w_scen_0.inc
$include w_scen_0.inc
;
parameter wind_scenarios(t,w,scen);
wind_scenarios(t,w,scen)$(wind_profile=0)=mnozitelj*w_capacity(w)*wind_scenarios_0(t,w,scen);
wind_scenarios(t,w,scen)$(wind_profile=1)=mnozitelj*w_capacity(w)*wind_scenarios_1(t,w,scen);
$OFFTEXT
