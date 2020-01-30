# load "ref0074-7782-77-6.gnu"
# chem = "nitrous acid"

set terminal postscript eps color
set title "ref = 74; chem = nitrous acid; casrn = 7782-77-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4725360     * exp(  -4747.123    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.4796447     point
set label "" at    293.1500    ,   0.6089317     point
set label "" at    288.1500    ,   0.8211202     point
set label "" at    283.1500    ,    1.095485     point
set label "" at    278.1500    ,    1.490254     point
set label "" at    273.1500    ,    2.033062     point
set label "" at    298.1500    ,   0.4725360     point ps 2 pt 6

plot [270:300] H(T)
