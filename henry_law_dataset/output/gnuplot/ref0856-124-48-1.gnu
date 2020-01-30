# load "ref0856-124-48-1.gnu"
# chem = "dibromochloromethane"

set terminal postscript eps color
set title "ref = 856; chem = dibromochloromethane; casrn = 124-48-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9344271E-02 * exp(  -4646.991    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1233654E-01 point
set label "" at    303.1500    ,   0.7049452E-02 point
set label "" at    313.1500    ,   0.4486015E-02 point
set label "" at    298.1500    ,   0.9344271E-02 point ps 2 pt 6

plot [290:320] H(T)
