# load "ref0856-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 856; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1104340E-02 * exp(  -4155.068    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1409890E-02 point
set label "" at    303.1500    ,   0.8657222E-03 point
set label "" at    313.1500    ,   0.5704759E-03 point
set label "" at    298.1500    ,   0.1104340E-02 point ps 2 pt 6

plot [290:320] H(T)
