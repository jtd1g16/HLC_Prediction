# load "ref0856-75-34-3.gnu"
# chem = "1,1-dichloroethane"

set terminal postscript eps color
set title "ref = 856; chem = 1,1-dichloroethane; casrn = 75-34-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1735788E-02 * exp(  -3656.021    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.2145485E-02 point
set label "" at    303.1500    ,   0.1409890E-02 point
set label "" at    313.1500    ,   0.9675718E-03 point
set label "" at    298.1500    ,   0.1735788E-02 point ps 2 pt 6

plot [290:320] H(T)
