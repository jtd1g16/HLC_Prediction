# load "ref0863-75-09-2.gnu"
# chem = "dichloromethane"

set terminal postscript eps color
set title "ref = 863; chem = dichloromethane; casrn = 75-09-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4117855E-02 * exp(  -3960.514    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1416483E-01 point
set label "" at    283.1500    ,   0.8157266E-02 point
set label "" at    293.1500    ,   0.5054716E-02 point
set label "" at    303.1500    ,   0.3381431E-02 point
set label "" at    298.1500    ,   0.4117855E-02 point ps 2 pt 6

plot [270:310] H(T)
