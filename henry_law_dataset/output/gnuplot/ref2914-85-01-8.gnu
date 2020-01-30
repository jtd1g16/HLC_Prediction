# load "ref2914-85-01-8.gnu"
# chem = "phenanthrene"

set terminal postscript eps color
set title "ref = 2914; chem = phenanthrene; casrn = 85-01-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1587302     * exp(  -7570.420    *(1/   298.    -1/T))

set label "" at    277.1500    ,    1.086957     point
set label "" at    298.1500    ,   0.1587302     point
set label "" at    298.1500    ,   0.1587302     point ps 2 pt 6

plot [270:300] H(T)
