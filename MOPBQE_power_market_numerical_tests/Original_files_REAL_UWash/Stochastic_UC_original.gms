$ONTEXT
GAMS code "Stochastic UC" by:

H. Pandzic, Y. Dvorkin, T. Qiu, Y. Wang, and D. Kirschen.
Unit Commitment under Uncertainty - GAMS Models
Library of the Renewable Energy Analysis Lab (REAL)
University of Washington, Seattle, USA.
Available at: http://www.ee.washington.edu/research/real/gams_code.html.

Downloaded Dec 24, 2015, by DH
$OFFTEXT

***************************************************************
*** PARAMETERS
***************************************************************

$include Input_Data.gms


***************************************************************
*** VARIABLES
***************************************************************

variable obj objective function variable

variable c_aux(t) auxilliary variable

variable c(t,scen,i) operation cost in each time period

positive variable g(t,scen,i) generator outputs

positive variable g_lin(t,scen,i,b) generator block outputs

binary variable suc(t,i,j) start up cost

variable pf(t,scen,l) power flow through lines

binary variable x(t,i) binary variable equal to 1 if generator is producing, and 0 otherwise

binary variable y(t,i) binary variable equal to 1 if generator is start-up, and 0 otherwise

binary variable z(t,i) binary variable equal to 1 if generator is shut-down, and 0 otherwise

variable theta(t,scen,s) bus voltage angles

positive variable curt(t,scen,w) wind curtailment

*positive variable ll(t,scen,s) unserved load

***************************************************************
*** EQUATION DECLARATION
***************************************************************

equations

cost objective function
cost_aux(t) auxilliary equation
bin_set1(t,i) setting start-up binary variables
bin_set10(t,i) setting start-up binary variables
bin_set2(t,i) setting start-up binary variables
gen_sum(t,scen,i) summing the generation of blocks per generator
gen_min(t,scen,i) genertor minimum output
cost_sum(t,scen,i) generation cost summation
block_output(t,scen,i,b) limiting the output of each generator block
min_updown_1(t,i) minimum updown time constraint 1
min_updown_2(t,i) minimum updown time constraint 2
min_updown_3(t,i) minimum updown time constraint 3
ramp_limit_min(t,scen,i) ramp-down limit
ramp_limit_max(t,scen,i) ramp-up limit
ramp_limit_min_1(scen,i) ramp-down limit for the first time period
ramp_limit_max_1(scen,i) ramp-up limit for the first time period
start_up_cost1(t,i,j) stairwise linear cost function - equation 1
start_up_cost2(t,i) stairwise linear cost function - equation 2
power_balance(t,scen,s) power balance for each bus
line_flow(t,scen,l) defining power flow through lines
line_capacity_min(t,scen,l) line capacitiy negative limit
line_capacity_max(t,scen,l) line capacitiy positive limit
voltage_angles_min(t,scen,s) voltage angles negative limit
voltage_angles_max(t,scen,s) voltage angles positive limit
curt_cons(t,scen,w) wind curtailment constraint
;

***************************************************************
*** SETTINGS
***************************************************************

*setting the reference bus
theta.fx (t,scen,'s101') = 0;

*needed for running twice through the same set in a single equation
alias (t,tt);

***************************************************************
*** EQUATIONS
***************************************************************

cost..
         obj =e= sum(t,c_aux(t));

cost_aux(t)..
         c_aux(t) =e= sum(scen,(sum(i,c(t,scen,i))+sum(w,curt(t,scen,w))*ws_penalty
)*prob(scen))
*+sum(s,ll(t,s)*voll)*prob(scen));
         ;

bin_set1(t,i)$(ord(t) gt 1)..
         y(t,i) - z(t,i) =e= x(t,i) - x(t-1,i);

bin_set10(t,i)$(ord(t) = 1)..
         y(t,i) - z(t,i) =e= x(t,i) - onoff_t0(i);

bin_set2(t,i)..
         y(t,i) + z(t,i) =l= 1;

cost_sum(t,scen,i)..
         c(t,scen,i) =e= a(i)*x(t,i) + sum(b,g_lin(t,scen,i,b)*k(i,b)) + sum(j,suc_sw(i,j)*suc(t,i,j));

gen_sum(t,scen,i)..
         g(t,scen,i) =e= sum(b,g_lin(t,scen,i,b));

gen_min(t,scen,i)..
         g(t,scen,i) =g= g_min(i)*x(t,i);

block_output(t,scen,i,b)..
         g_lin(t,scen,i,b) =l= g_max(i,b)*x(t,i);

min_updown_1(t,i)$(L_up_min(i)+L_down_min(i) gt 0 and ord(t) le L_up_min(i)+L_down_min(i))..
         x(t,i) =e= onoff_t0(i);

min_updown_2(t,i)..
         sum(tt$(ord(tt) ge ord(t)-g_up(i)+1 and ord(tt) le ord(t)),y(tt,i)) =l= x(t,i);

min_updown_3(t,i)..
         sum(tt$(ord(tt) ge ord(t)-g_down(i)+1 and ord(tt) le ord(t)),z(tt,i)) =l= 1-x(t,i);

ramp_limit_min(t,scen,i)$(ord(t) gt 1)..
         -ramp_down(i) =l= g(t,scen,i) - g(t-1,scen,i);

ramp_limit_max(t,scen,i)$(ord(t) gt 1)..
         ramp_up(i) =g= g(t,scen,i) - g(t-1,scen,i);

ramp_limit_min_1(scen,i)..
         -ramp_down(i) =l= g('t1',scen,i) - g_0(i);

ramp_limit_max_1(scen,i)..
         ramp_up(i) =g= g('t1',scen,i) - g_0(i);

start_up_cost1(t,i,j)..
         suc(t,i,j) =l= sum(tt$(ord(tt) lt ord(t) and ord(tt) ge suc_sl(i,j) and ord(tt) le suc_sl(i,j+1)-1),z(t-ord(j),i))+
         1$(ord(j) lt card(j) and count_off_init(i)+ord(t)-1 ge suc_sl(i,j) and count_off_init(i)+ord(t)-1 lt suc_sl(i,j+1))+
         1$(ord(j) = card(j) and count_off_init(i)+ord(t)-1 ge suc_sl(i,j));

start_up_cost2(t,i)..
         sum(j,suc(t,i,j)) =e= y(t,i);

power_balance(t,scen,s)..
         sum(i$(gen_map(i,s)),g(t,scen,i)) + sum(w$(w_map(w,s)),wind_scenarios(t,w,scen)-curt(t,scen,w)) -
         sum(l$(line_map(l,s) <> 0),pf(t,scen,l)*line_map(l,s)) =e= d(t,s)
*-ll(t,scen,s)
         ;

line_flow(t,scen,l)..
         pf(t,scen,l) =e= admitance(l)*sum(s$(line_map(l,s) <> 0),theta(t,scen,s)*line_map(l,s));

line_capacity_min(t,scen,l)..
         pf(t,scen,l) =g= -l_max(l)*line_capacity;

line_capacity_max(t,scen,l)..
         pf(t,scen,l) =l= l_max(l)*line_capacity;

voltage_angles_min(t,scen,s)..
         theta(t,scen,s) =g= -pi;

voltage_angles_max(t,scen,s)..
         theta(t,scen,s) =l= pi;

curt_cons(t,scen,w)..
         curt(t,scen,w) =l= w_det(t,w);

***************************************************************
*** SOLVE
***************************************************************

model ep /all/;

option reslim = 86400;
option Savepoint=1;
option optcr=0.005;
ep.optfile = 1;

ep.limrow =0;
ep.limcol =0;

file opt cplex option file /cplex.opt/;
put opt;
put 'threads 4'/;
put 'miptrace _UC_stochastic_4_04_0.csv'/;
putclose;

solve ep using mip minimizing obj;

parameter overall_suc, output_cost,time_elapsed;

overall_suc=sum((t,i,j), suc_sw(i,j)*suc.l(t,i,j));
output_cost=sum((t,scen,i), (a(i)*x.l(t,i) + sum(b,g_lin.l(t,scen,i,b)*k(i,b)))*prob(scen));
time_elapsed = ep.etSolver;

wind_scenarios(t,w,scen)=wind_scenarios(t,w,scen)+Eps;
display mnozitelj;

Execute_Unload '_UC_stochastic_4_04_0', obj, c_aux, c, g,x, y, z, curt, overall_suc,output_cost,time_elapsed,wind_scenarios;
