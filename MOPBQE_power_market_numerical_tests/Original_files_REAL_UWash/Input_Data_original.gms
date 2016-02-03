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
*** SETS
***************************************************************

set t time periods /t1*t24/;
set i generators /i1*i96/;
set b generator blocks /b1*b3/;
set s buses /s101*s124,s201*s224,s301*s325/;
set l lines /l1*l120/;
set j start up cost intervals /j1*j8/;
set w wind power plant /w1*w19/;
set robust upper and lower bound /col1*col2/;
set u 5 "scenarios" for improved interval optimization /u1*u5/;
set scen scenarios /scen1*scen10/;
set column auxiliary /column1/;
set tr transmission data (1-HP 2-MOV 3-MS 4-YD) /tr1*tr4/;


***************************************************************
*** OPTIONS
***************************************************************

scalar transmission_option/4/;
* 1 - HP
* 2 - MOV
* 3 - MS
* 4 - YD

scalar wind_energy_penetration <1 /0.5/;

scalar wind_profile 1-favorable 0-unfavorable /1/;

parameter line_capacity line capacity factor /0.8/;

parameter ws_penalty/0/;


***************************************************************
*** PARAMETERS
***************************************************************


*GENERATOR DATA

table gen_map(i,s) generator map
$call =xls2gms r=g_map!a1:bv97 i=input_UC_ii.xlsx o=gmap.inc
$include gmap.inc
;

table g_max(i,b) generator block generation limit
$call =xls2gms r=generators_c!be2:bh98 i=input_UC_ii.xlsx o=block_max.inc
$include block_max.inc
;

table k_option(i,b,tr) slope of each generator cost curve block
$call =xls2gms r=generators_c!aw2:bc290 i=input_UC_ii.xlsx o=k.inc
$include k.inc
;
parameter k(i,b);
k(i,b)=sum(tr$(ord(tr)=transmission_option),k_option(i,b,tr));

table suc_sw_option(i,j,tr) generator stepwise start-up cost
$call =xls2gms r=generators_c!bj2:bp770 i=input_UC_ii.xlsx o=start_up_sw.inc
$include start_up_sw.inc
;
parameter suc_sw(i,j);
suc_sw(i,j)=sum(tr$(ord(tr)=transmission_option),suc_sw_option(i,j,tr));

table suc_sl(i,j) generator stepwise start-up hourly blocks
$call =xls2gms r=generators_c!br2:bz98 i=input_UC_ii.xlsx o=start_up_sl.inc
$include start_up_sl.inc
;

table aux2(i,column)
$call =xls2gms r=generators_c!d2:e98 i=input_UC_ii.xlsx o=aux2.inc
$include aux2.inc
;
parameter count_off_init(i) number of time periods each generator has been off;
count_off_init(i)=sum(column,aux2(i,column));

table aux3(i,column)
$call =xls2gms r=generators_c!g2:h98 i=input_UC_ii.xlsx o=aux3.inc
$include aux3.inc
;
parameter count_on_init(i) number of time periods each generator has been on;
count_on_init(i)=sum(column,aux3(i,column));

table aux4(i,tr)
$call =xls2gms r=generators_c!j2:N98 i=input_UC_ii.xlsx o=aux4.inc
$include aux4.inc
;
parameter a(i) fixed operating cost of each generator;
a(i)=sum(tr$(ord(tr)=transmission_option),aux4(i,tr));

table aux5(i,tr)
$call =xls2gms r=generators_c!p2:t98 i=input_UC_ii.xlsx o=aux5.inc
$include aux5.inc
;
parameter ramp_up(i) generator ramp-up limit;
ramp_up(i)=sum(tr$(ord(tr)=transmission_option),aux5(i,tr));

table aux6(i,tr)
$call =xls2gms r=generators_c!v2:z98 i=input_UC_ii.xlsx o=aux6.inc
$include aux6.inc
;
parameter ramp_down(i) generator ramp-down limit;
ramp_down(i)=sum(tr$(ord(tr)=transmission_option),aux6(i,tr));

table aux7(i,tr)
$call =xls2gms r=generators_c!ab2:af98 i=input_UC_ii.xlsx o=aux7.inc
$include aux7.inc
;
parameter g_down(i) generator minimum down time;
g_down(i)=sum(tr$(ord(tr)=transmission_option),aux7(i,tr));

table aux8(i,tr)
$call =xls2gms r=generators_c!ah2:al98 i=input_UC_ii.xlsx o=aux8.inc
$include aux8.inc
;
parameter g_up(i) generator minimum up time;
g_up(i)=sum(tr$(ord(tr)=transmission_option),aux8(i,tr));

table aux9(i,tr)
$call =xls2gms r=generators_c!an2:ar98 i=input_UC_ii.xlsx o=aux9.inc
$include aux9.inc
;
parameter g_min(i) generator minimum output;
g_min(i)=sum(tr$(ord(tr)=transmission_option),aux9(i,tr));

table aux10(i,column)
$call =xls2gms r=generators_c!at2:au98 i=input_UC_ii.xlsx o=aux10.inc
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



*LINE DATA

table aux11(l,column)
$call =xls2gms r=line_admittance!a2:b122 i=input_UC_ii.xlsx o=aux11.inc
$include aux11.inc
;
parameter admitance(l) line admittance;
admitance(l)=sum(column,aux11(l,column));


table line_map(l,s) line map
$call =xls2gms r=line_map!a1:bv121 i=input_UC_ii.xlsx o=line_map.inc
$include line_map.inc
;


table aux12(l,column)
$call =xls2gms r=line_admittance!d2:e122 i=input_UC_ii.xlsx o=aux12.inc
$include aux12.inc
;
parameter l_max(l) line capacities (long-term ratings);
l_max(l)=sum(column,aux12(l,column));



*DEMAND DATA


table d(t,s) yearly demand at bus s
*$call =xls2gms r=load!k2:cf38 i=input_UC_ii.xlsx o=load.inc
$call =xls2gms r=load!k2:cf26 i=input_UC_ii.xlsx o=load24.inc
$include load24.inc
;



*WIND DATA


table w_map(w,s) wind map
$call =xls2gms r=wind_map!a1:bv20 i=input_UC_ii.xlsx o=w_map.inc
$include w_map.inc
;

table aux21(w,column) wind power plant capacities
$call =xls2gms r=wind_test!aa4:ab23 i=input_UC_ii.xlsx o=w_capacities.inc
$include w_capacities.inc
;
parameter w_capacity(w) line admittance;
w_capacity(w)=sum(column,aux21(w,column));

table w_det_pu_1(t,w) available wind generation
$call =xls2gms r=wind_data!a5:t29 i=input_UC_ii.xlsx o=w_det1.inc
$include w_det1.inc
;
table w_det_pu_0(t,w) available wind generation
$call =xls2gms r=wind_data!w5:ap29 i=input_UC_ii.xlsx o=w_det0.inc
$include w_det0.inc
;

scalar mnozitelj;
mnozitelj$(wind_profile=0)=sum((t,s),d(t,s))*wind_energy_penetration/sum((t,w),w_capacity(w)*w_det_pu_0(t,w));
mnozitelj$(wind_profile=1)=sum((t,s),d(t,s))*wind_energy_penetration/sum((t,w),w_capacity(w)*w_det_pu_1(t,w));


parameter w_det(t,w);
w_det(t,w)$(wind_profile=0)=mnozitelj*w_capacity(w)*w_det_pu_0(t,w);
w_det(t,w)$(wind_profile=1)=mnozitelj*w_capacity(w)*w_det_pu_1(t,w);


table wind_robust_pu_1(t,w,robust)
$call =xls2gms r=wind_data!a35:e491 i=input_UC_ii.xlsx o=w_robust_1.inc
$include w_robust_1.inc
;
table wind_robust_pu_0(t,w,robust)
$call =xls2gms r=wind_data!w35:aa491 i=input_UC_ii.xlsx o=w_robust_0.inc
$include w_robust_0.inc
;
parameter wind_robust(t,w,robust);
wind_robust(t,w,robust)$(wind_profile=0)=mnozitelj*w_capacity(w)*wind_robust_pu_0(t,w,robust);
wind_robust(t,w,robust)$(wind_profile=1)=mnozitelj*w_capacity(w)*wind_robust_pu_1(t,w,robust);


table wind_stoch_max_1(t,w,robust)
$call =xls2gms r=wind_data!h35:l491 i=input_UC_ii.xlsx o=w_stoch_1.inc
$include w_stoch_1.inc
;
table wind_stoch_max_0(t,w,robust)
$call =xls2gms r=wind_data!ad35:ah491 i=input_UC_ii.xlsx o=w_stoch_0.inc
$include w_stoch_0.inc
;
parameter wind_stoch_max(t,w,robust);
wind_stoch_max(t,w,robust)$(wind_profile=0)=mnozitelj*w_capacity(w)*wind_stoch_max_0(t,w,robust);
wind_stoch_max(t,w,robust)$(wind_profile=1)=mnozitelj*w_capacity(w)*wind_stoch_max_1(t,w,robust);


table prob_aux(scen,column)
$call =xls2gms r=wind_test!ad4:ae14 i=input_UC_ii.xlsx o=prob_aux.inc
$include prob_aux.inc
;
parameter prob(scen) stochastic scenario probabilities;
prob(scen)=sum(column,prob_aux(scen,column));

table wind_scenarios_1(t,w,scen)
$call =xls2gms r=wind_data!a496:m952 i=input_UC_ii.xlsx o=w_scen_1.inc
$include w_scen_1.inc
;
table wind_scenarios_0(t,w,scen)
$call =xls2gms r=wind_data!w496:ai952 i=input_UC_ii.xlsx o=w_scen_0.inc
$include w_scen_0.inc
;
parameter wind_scenarios(t,w,scen);
wind_scenarios(t,w,scen)$(wind_profile=0)=mnozitelj*w_capacity(w)*wind_scenarios_0(t,w,scen);
wind_scenarios(t,w,scen)$(wind_profile=1)=mnozitelj*w_capacity(w)*wind_scenarios_1(t,w,scen);
