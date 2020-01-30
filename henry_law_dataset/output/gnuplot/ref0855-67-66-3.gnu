# load "ref0855-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 855; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2593984E-02 * exp(  -3882.305    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.3255406E-02 point
set label "" at    308.1500    ,   0.1682125E-02 point
set label "" at    323.1500    ,   0.9525286E-03 point
set label "" at    298.1500    ,   0.2593984E-02 point ps 2 pt 6

plot [290:330] H(T)
