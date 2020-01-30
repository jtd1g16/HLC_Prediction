# load "ref0856-75-35-4.gnu"
# chem = "1,1-dichloroethene"

set terminal postscript eps color
set title "ref = 856; chem = 1,1-dichloroethene; casrn = 75-35-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3547239E-03 * exp(  -3349.790    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.4309709E-03 point
set label "" at    303.1500    ,   0.2928556E-03 point
set label "" at    313.1500    ,   0.2077733E-03 point
set label "" at    298.1500    ,   0.3547239E-03 point ps 2 pt 6

plot [290:320] H(T)
