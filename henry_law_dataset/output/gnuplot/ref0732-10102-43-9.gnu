# load "ref0732-10102-43-9.gnu"
# chem = "nitrogen monoxide"

set terminal postscript eps color
set title "ref = 732; chem = nitrogen monoxide; casrn = 10102-43-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1892963E-04 * exp(  -1662.990    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3240931E-04 point
set label "" at    274.1500    ,   0.3154244E-04 point
set label "" at    275.1500    ,   0.3070255E-04 point
set label "" at    276.1500    ,   0.2989331E-04 point
set label "" at    277.1500    ,   0.2913585E-04 point
set label "" at    278.1500    ,   0.2836535E-04 point
set label "" at    279.1500    ,   0.2765111E-04 point
set label "" at    280.1500    ,   0.2695795E-04 point
set label "" at    281.1500    ,   0.2632203E-04 point
set label "" at    282.1500    ,   0.2567090E-04 point
set label "" at    283.1500    ,   0.2509664E-04 point
set label "" at    284.1500    ,   0.2453591E-04 point
set label "" at    285.1500    ,   0.2401634E-04 point
set label "" at    286.1500    ,   0.2353299E-04 point
set label "" at    287.1500    ,   0.2306615E-04 point
set label "" at    288.1500    ,   0.2261747E-04 point
set label "" at    289.1500    ,   0.2218947E-04 point
set label "" at    290.1500    ,   0.2181285E-04 point
set label "" at    291.1500    ,   0.2140124E-04 point
set label "" at    292.1500    ,   0.2104108E-04 point
set label "" at    293.1500    ,   0.2069800E-04 point
set label "" at    294.1500    ,   0.2034597E-04 point
set label "" at    295.1500    ,   0.1999896E-04 point
set label "" at    296.1500    ,   0.1966173E-04 point
set label "" at    297.1500    ,   0.1934740E-04 point
set label "" at    298.1500    ,   0.1903597E-04 point
set label "" at    299.1500    ,   0.1873611E-04 point
set label "" at    300.1500    ,   0.1845128E-04 point
set label "" at    301.1500    ,   0.1817339E-04 point
set label "" at    302.1500    ,   0.1791070E-04 point
set label "" at    303.1500    ,   0.1765550E-04 point
set label "" at    298.1500    ,   0.1892963E-04 point ps 2 pt 6

plot [270:310] H(T)
