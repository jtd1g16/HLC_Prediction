# load "ref0855-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 855; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1052797E-02 * exp(  -4243.533    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1317665E-02 point
set label "" at    303.1500    ,   0.8647173E-03 point
set label "" at    313.1500    ,   0.5220935E-03 point
set label "" at    298.1500    ,   0.1052797E-02 point ps 2 pt 6

plot [290:320] H(T)
