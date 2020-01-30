# load "ref2905-75-09-2.gnu"
# chem = "dichloromethane"

set terminal postscript eps color
set title "ref = 2905; chem = dichloromethane; casrn = 75-09-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4335416E-02 * exp(  -3483.351    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.6635700E-02 point
set label "" at    298.1500    ,   0.4158004E-02 point
set label "" at    308.1500    ,   0.3032141E-02 point
set label "" at    298.1500    ,   0.4335416E-02 point ps 2 pt 6

plot [280:310] H(T)
