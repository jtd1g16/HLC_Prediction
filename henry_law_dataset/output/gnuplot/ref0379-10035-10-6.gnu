# load "ref0379-10035-10-6.gnu"
# chem = "hydrogen bromide"

set terminal postscript eps color
set title "ref = 379; chem = hydrogen bromide; casrn = 10035-10-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2424805     * exp(  -367.5953    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2711077     point
set label "" at    283.1500    ,   0.2594055     point
set label "" at    298.1500    ,   0.2422604     point
set label "" at    298.1500    ,   0.2424805     point ps 2 pt 6

plot [270:300] H(T)
