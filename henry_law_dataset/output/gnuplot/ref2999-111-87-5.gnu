# load "ref2999-111-87-5.gnu"
# chem = "1-octanol"

set terminal postscript eps color
set title "ref = 2999; chem = 1-octanol; casrn = 111-87-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2097699     * exp(  -6876.572    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.2222222     point
set label "" at    305.5500    ,   0.1176471     point
set label "" at    323.1500    ,   0.3182686E-01 point
set label "" at    343.1500    ,   0.1085776E-01 point
set label "" at    298.1500    ,   0.2097699     point ps 2 pt 6

plot [290:350] H(T)
