# load "ref2999-143-08-8.gnu"
# chem = "1-nonanol"

set terminal postscript eps color
set title "ref = 2999; chem = 1-nonanol; casrn = 143-08-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1087521     * exp(  -6280.756    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1176471     point
set label "" at    305.5500    ,   0.6369427E-01 point
set label "" at    323.1500    ,   0.1848429E-01 point
set label "" at    343.1500    ,   0.7501875E-02 point
set label "" at    298.1500    ,   0.1087521     point ps 2 pt 6

plot [290:350] H(T)
