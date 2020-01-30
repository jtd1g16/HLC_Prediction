# load "ref0863-75-03-6.gnu"
# chem = "iodoethane"

set terminal postscript eps color
set title "ref = 863; chem = iodoethane; casrn = 75-03-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1461273E-02 * exp(  -4042.804    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.5169814E-02 point
set label "" at    283.1500    ,   0.2923445E-02 point
set label "" at    293.1500    ,   0.1803424E-02 point
set label "" at    303.1500    ,   0.1195955E-02 point
set label "" at    298.1500    ,   0.1461273E-02 point ps 2 pt 6

plot [270:310] H(T)
