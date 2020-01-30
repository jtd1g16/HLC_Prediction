# load "ref2464-98-55-5.gnu"
# chem = "alpha-terpineol"

set terminal postscript eps color
set title "ref = 2464; chem = alpha-terpineol; casrn = 98-55-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7371341     * exp(  -5444.251    *(1/   298.    -1/T))

set label "" at    279.1500    ,    2.554524     point
set label "" at    296.6500    ,   0.8084359     point
set label "" at    298.1500    ,   0.7371341     point ps 2 pt 6

plot [270:300] H(T)
