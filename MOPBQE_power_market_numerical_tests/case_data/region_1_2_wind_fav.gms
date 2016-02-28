$ONTEXT
Case region_1_2_wind_fav of the numerical test runs
 
Use 12 time steps (2 hours each), include regions 1 and 2 of the overall dataset (48 nodes)
Assume favourable wind conditions

$OFFTEXT

$SETGLOBAL duration '12'

scalar wind_energy_penetration <1 / 0.99 /;

scalar wind_profile 1-favorable 0-unfavorable / 1 /;

parameter line_capacity line capacity factor / 0.85 /;

timestep = 1.9 ;

* use a 48 node network (subset of full grid)
*n(n_s)    = yes ;
n('s101') = yes ;
n('s102') = yes ;
n('s103') = yes ;
n('s104') = yes ;
n('s105') = yes ;
n('s106') = yes ;
n('s107') = yes ;
n('s108') = yes ;
n('s109') = yes ;
n('s110') = yes ;
n('s111') = yes ;
n('s112') = yes ;
n('s113') = yes ;
n('s114') = yes ;
n('s115') = yes ;
n('s116') = yes ;
n('s117') = yes ;
n('s118') = yes ;
n('s119') = yes ;
n('s120') = yes ;
n('s121') = yes ;
n('s122') = yes ;
n('s123') = yes ;
n('s124') = yes ;
n('s201') = yes ;
n('s202') = yes ;
n('s203') = yes ;
n('s204') = yes ;
n('s205') = yes ;
n('s206') = yes ;
n('s207') = yes ;
n('s208') = yes ;
n('s209') = yes ;
n('s210') = yes ;
n('s211') = yes ;
n('s212') = yes ;
n('s213') = yes ;
n('s214') = yes ;
n('s215') = yes ;
n('s216') = yes ;
n('s217') = yes ;
n('s218') = yes ;
n('s219') = yes ;
n('s220') = yes ;
n('s221') = yes ;
n('s222') = yes ;
n('s223') = yes ;
n('s224') = yes ;

* phase angle at slack bus equals 0 by defintion - necessary for tractability of DCLF approach
slack('s101') = 1 ;

g_max(i_s,b) = g_max(i_s,b) * 1.5 ;
g_min(i_s)   = g_min(i_s) * 1.5 ;

* ensure that the first load block can satisfy minimum demand
g_max(i_s,'b2')$( g_max(i_s,'b1') le g_min(i_s) ) = g_max(i_s,'b2') + g_max(i_s,'b1') - g_min(i_s) ;
g_max(i_s,'b1')$( g_max(i_s,'b1') le g_min(i_s) ) = g_min(i_s) ;

c_on(i_s) = c_on(i_s) ;

* assumption on shut-down costs (linear ramp-down from minimum output level at no-load cost)
c_off(i_s) = ( c_NL(i_s) * g_min(i_s) / ramp_down(i_s) + c_G(i_s,'b1') * g_min(i_s) * g_min(i_s) / ramp_down(i_s) / 2 ) ;

*** line parameters ***

f_max(l_s) = f_max(l_s) * line_capacity ;
*f_max('l7') = 80 ;

*** demand parameters ***

d_max('t1',n) = d_max_full('d1','t2',n) ;
d_max('t2',n) = d_max_full('d1','t4',n) ;
d_max('t3',n) = d_max_full('d1','t6',n) ;
d_max('t4',n) = d_max_full('d1','t8',n) ;
d_max('t5',n) = d_max_full('d1','t10',n) ;
d_max('t6',n) = d_max_full('d1','t12',n) ;
d_max('t7',n) = d_max_full('d1','t14',n) ;
d_max('t8',n) = d_max_full('d1','t16',n) ;
d_max('t9',n) = d_max_full('d1','t18',n) ;
d_max('t10',n) = d_max_full('d1','t20',n) ;
d_max('t11',n) = d_max_full('d1','t22',n) ;
d_max('t12',n) = d_max_full('d1','t24',n) ;

* sclaing to match generator capacity
d_max(t_s,n) = d_max(t_s,n) * 1 ;

u_D(t_s,n) = 250 ;
price_bound = 1000 ;
