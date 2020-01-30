# load "ref0764-7440-59-7.gnu"
# chem = "helium"

set terminal postscript eps color
set title "ref = 764; chem = helium; casrn = 7440-59-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3717395E-05 * exp(  -357.4662    *(1/   298.    -1/T))

set label "" at    284.1500    ,   0.3968517E-05 point
set label "" at    285.1500    ,   0.3937787E-05 point
set label "" at    286.3500    ,   0.3893888E-05 point
set label "" at    289.7500    ,   0.3823648E-05 point
set label "" at    294.8500    ,   0.3744629E-05 point
set label "" at    297.8500    ,   0.3718290E-05 point
set label "" at    300.5500    ,   0.3709510E-05 point
set label "" at    298.1500    ,   0.3717395E-05 point ps 2 pt 6

plot [280:310] H(T)
