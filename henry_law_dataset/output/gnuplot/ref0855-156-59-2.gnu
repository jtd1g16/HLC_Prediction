# load "ref0855-156-59-2.gnu"
# chem = "(Z)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 855; chem = (Z)-1,2-dichloroethene; casrn = 156-59-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2406089E-02 * exp(  -3757.876    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.3007712E-02 point
set label "" at    303.1500    ,   0.1921594E-02 point
set label "" at    313.1500    ,   0.1327144E-02 point
set label "" at    298.1500    ,   0.2406089E-02 point ps 2 pt 6

plot [290:320] H(T)
