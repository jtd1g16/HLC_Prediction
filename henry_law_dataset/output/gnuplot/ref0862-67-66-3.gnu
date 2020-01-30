# load "ref0862-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 862; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2229345E-02 * exp(  -4715.917    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1016690E-01 point
set label "" at    283.1500    ,   0.4746608E-02 point
set label "" at    293.1500    ,   0.2765186E-02 point
set label "" at    303.1500    ,   0.1836118E-02 point
set label "" at    298.1500    ,   0.2229345E-02 point ps 2 pt 6

plot [270:310] H(T)
