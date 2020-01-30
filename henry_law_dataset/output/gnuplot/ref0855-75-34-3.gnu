# load "ref0855-75-34-3.gnu"
# chem = "1,1-dichloroethane"

set terminal postscript eps color
set title "ref = 855; chem = 1,1-dichloroethane; casrn = 75-34-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1636143E-02 * exp(  -3609.117    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.2088374E-02 point
set label "" at    308.1500    ,   0.1000758E-02 point
set label "" at    318.1500    ,   0.8126565E-03 point
set label "" at    298.1500    ,   0.1636143E-02 point ps 2 pt 6

plot [290:320] H(T)
