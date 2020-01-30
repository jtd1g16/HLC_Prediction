# load "ref0379-74-84-0.gnu"
# chem = "ethane"

set terminal postscript eps color
set title "ref = 379; chem = ethane; casrn = 74-84-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1794454E-04 * exp(  -2779.525    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4347682E-04 point
set label "" at    274.1500    ,   0.4172436E-04 point
set label "" at    275.1500    ,   0.4003795E-04 point
set label "" at    276.1500    ,   0.3841759E-04 point
set label "" at    277.1500    ,   0.3686327E-04 point
set label "" at    278.1500    ,   0.3537060E-04 point
set label "" at    279.1500    ,   0.3394397E-04 point
set label "" at    280.1500    ,   0.3258340E-04 point
set label "" at    281.1500    ,   0.3128887E-04 point
set label "" at    282.1500    ,   0.3005598E-04 point
set label "" at    283.1500    ,   0.2888914E-04 point
set label "" at    284.1500    ,   0.2786321E-04 point
set label "" at    285.1500    ,   0.2688571E-04 point
set label "" at    286.1500    ,   0.2595224E-04 point
set label "" at    287.1500    ,   0.2507160E-04 point
set label "" at    288.1500    ,   0.2423500E-04 point
set label "" at    289.1500    ,   0.2345124E-04 point
set label "" at    290.1500    ,   0.2271591E-04 point
set label "" at    291.1500    ,   0.2202902E-04 point
set label "" at    292.1500    ,   0.2139056E-04 point
set label "" at    293.1500    ,   0.2080054E-04 point
set label "" at    294.1500    ,   0.2020611E-04 point
set label "" at    295.1500    ,   0.1963370E-04 point
set label "" at    296.1500    ,   0.1908771E-04 point
set label "" at    297.1500    ,   0.1856813E-04 point
set label "" at    298.1500    ,   0.1807058E-04 point
set label "" at    299.1500    ,   0.1759944E-04 point
set label "" at    300.1500    ,   0.1715031E-04 point
set label "" at    301.1500    ,   0.1672761E-04 point
set label "" at    302.1500    ,   0.1633133E-04 point
set label "" at    303.1500    ,   0.1595706E-04 point
set label "" at    298.1500    ,   0.1794454E-04 point ps 2 pt 6

plot [270:310] H(T)
