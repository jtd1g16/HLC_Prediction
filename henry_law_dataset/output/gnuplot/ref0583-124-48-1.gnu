# load "ref0583-124-48-1.gnu"
# chem = "dibromochloromethane"

set terminal postscript eps color
set title "ref = 583; chem = dibromochloromethane; casrn = 124-48-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7189676E-02 * exp(  -5178.986    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3550937E-01 point
set label "" at    283.1500    ,   0.1777262E-01 point
set label "" at    293.1500    ,   0.9745269E-02 point
set label "" at    298.1500    ,   0.7189676E-02 point ps 2 pt 6

plot [270:300] H(T)
