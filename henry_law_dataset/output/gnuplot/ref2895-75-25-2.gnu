# load "ref2895-75-25-2.gnu"
# chem = "tribromomethane"

set terminal postscript eps color
set title "ref = 2895; chem = tribromomethane; casrn = 75-25-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8505674E-02 * exp(  -1496.654    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.8430966E-02 point
set label "" at    308.1500    ,   0.7337726E-02 point
set label "" at    323.1500    ,   0.5731686E-02 point
set label "" at    298.1500    ,   0.8505674E-02 point ps 2 pt 6

plot [290:330] H(T)
