# load "ref2464-78-70-6.gnu"
# chem = "3,7-dimethyl-1,6-octadien-3-ol"

set terminal postscript eps color
set title "ref = 2464; chem = 3,7-dimethyl-1,6-octadien-3-ol; casrn = 78-70-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2066429     * exp(  -13735.69    *(1/   298.    -1/T))

set label "" at    279.1500    ,    4.753662     point
set label "" at    296.6500    ,   0.2608491     point
set label "" at    298.1500    ,   0.2066429     point ps 2 pt 6

plot [270:300] H(T)
