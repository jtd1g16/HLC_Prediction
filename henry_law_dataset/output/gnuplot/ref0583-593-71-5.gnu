# load "ref0583-593-71-5.gnu"
# chem = "chloroiodomethane"

set terminal postscript eps color
set title "ref = 583; chem = chloroiodomethane; casrn = 593-71-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8808378E-02 * exp(  -4572.480    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3609149E-01 point
set label "" at    283.1500    ,   0.1957445E-01 point
set label "" at    293.1500    ,   0.1152460E-01 point
set label "" at    298.1500    ,   0.8808378E-02 point ps 2 pt 6

plot [270:300] H(T)
