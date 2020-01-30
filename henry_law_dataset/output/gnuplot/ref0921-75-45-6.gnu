# load "ref0921-75-45-6.gnu"
# chem = "chlorodifluoromethane"

set terminal postscript eps color
set title "ref = 921; chem = chlorodifluoromethane; casrn = 75-45-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3635374E-03 * exp(  -2743.120    *(1/   298.    -1/T))

set label "" at    278.2400    ,   0.7315520E-03 point
set label "" at    278.2400    ,   0.7304898E-03 point
set label "" at    308.0600    ,   0.2492879E-03 point
set label "" at    308.0600    ,   0.2456365E-03 point
set label "" at    338.1100    ,   0.1271059E-03 point
set label "" at    338.1100    ,   0.1301856E-03 point
set label "" at    298.1500    ,   0.3635374E-03 point ps 2 pt 6

plot [270:340] H(T)
