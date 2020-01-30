# load "ref2445-151-67-7.gnu"
# chem = "1-bromo-1-chloro-2,2,2-trifluoroethane"

set terminal postscript eps color
set title "ref = 2445; chem = 1-bromo-1-chloro-2,2,2-trifluoroethane; casrn = 151-67-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5304683E-03 * exp(  -4984.089    *(1/   298.    -1/T))

set label "" at    277.1500    ,   0.1884553E-02 point
set label "" at    283.1500    ,   0.1285723E-02 point
set label "" at    293.1500    ,   0.7045059E-03 point
set label "" at    298.1500    ,   0.5283794E-03 point
set label "" at    303.1500    ,   0.4050909E-03 point
set label "" at    310.1500    ,   0.2773992E-03 point
set label "" at    298.1500    ,   0.5304683E-03 point ps 2 pt 6

plot [270:320] H(T)
