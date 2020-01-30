# load "ref0583-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 583; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2246753E-02 * exp(  -4109.136    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.7919356E-02 point
set label "" at    283.1500    ,   0.4678035E-02 point
set label "" at    293.1500    ,   0.2837316E-02 point
set label "" at    298.1500    ,   0.2246753E-02 point ps 2 pt 6

plot [270:300] H(T)
