# load "ref2905-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 2905; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2559034E-02 * exp(  -3356.848    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.3739716E-02 point
set label "" at    298.1500    ,   0.2619859E-02 point
set label "" at    308.1500    ,   0.1754386E-02 point
set label "" at    298.1500    ,   0.2559034E-02 point ps 2 pt 6

plot [280:310] H(T)
