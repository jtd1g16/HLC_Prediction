# load "ref0862-7783-06-4.gnu"
# chem = "hydrogen sulfide"

set terminal postscript eps color
set title "ref = 862; chem = hydrogen sulfide; casrn = 7783-06-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1030237E-02 * exp(  -2070.965    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2034701E-02 point
set label "" at    283.1500    ,   0.1480343E-02 point
set label "" at    293.1500    ,   0.1124568E-02 point
set label "" at    303.1500    ,   0.8867968E-03 point
set label "" at    313.1500    ,   0.7229992E-03 point
set label "" at    323.1500    ,   0.6058751E-03 point
set label "" at    333.1500    ,   0.5178118E-03 point
set label "" at    298.1500    ,   0.1030237E-02 point ps 2 pt 6

plot [270:340] H(T)
