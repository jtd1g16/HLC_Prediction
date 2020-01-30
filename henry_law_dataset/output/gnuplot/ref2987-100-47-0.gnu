# load "ref2987-100-47-0.gnu"
# chem = "benzenenitrile"

set terminal postscript eps color
set title "ref = 2987; chem = benzenenitrile; casrn = 100-47-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2911656     * exp(  -5140.222    *(1/   298.    -1/T))

set label "" at    283.0000    ,   0.7322971     point
set label "" at    288.0000    ,   0.5220824     point
set label "" at    293.0000    ,   0.4145078     point
set label "" at    298.0000    ,   0.2862077     point
set label "" at    298.1500    ,   0.2911656     point ps 2 pt 6

plot [280:300] H(T)
