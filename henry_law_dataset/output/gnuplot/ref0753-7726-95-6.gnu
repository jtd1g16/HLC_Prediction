# load "ref0753-7726-95-6.gnu"
# chem = "molecular bromine"

set terminal postscript eps color
set title "ref = 753; chem = molecular bromine; casrn = 7726-95-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7892791E-02 * exp(  -3854.992    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2741454E-01 point
set label "" at    283.1500    ,   0.1644872E-01 point
set label "" at    293.1500    ,   0.9310597E-02 point
set label "" at    298.1500    ,   0.7310543E-02 point
set label "" at    303.1500    ,   0.5874543E-02 point
set label "" at    313.1500    ,   0.3916362E-02 point
set label "" at    323.1500    ,   0.3710238E-02 point
set label "" at    333.1500    ,   0.1908942E-02 point
set label "" at    298.1500    ,   0.7892791E-02 point ps 2 pt 6

plot [270:340] H(T)
