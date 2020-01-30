# load "ref3116-637-92-3.gnu"
# chem = "ethyl t-butyl ether"

set terminal postscript eps color
set title "ref = 3116; chem = ethyl t-butyl ether; casrn = 637-92-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4446022E-02 * exp(  -4340.621    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.2390579E-02 point
set label "" at    323.0000    ,   0.1327144E-02 point
set label "" at    333.0000    ,   0.9301161E-03 point
set label "" at    343.0000    ,   0.7032009E-03 point
set label "" at    298.1500    ,   0.4446022E-02 point ps 2 pt 6

plot [310:350] H(T)
