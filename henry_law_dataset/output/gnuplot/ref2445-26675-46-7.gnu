# load "ref2445-26675-46-7.gnu"
# chem = "1-chloro-2,2,2-trifluoroethyl difluoromethyl ether"

set terminal postscript eps color
set title "ref = 2445; chem = 1-chloro-2,2,2-trifluoroethyl difluoromethyl ether; casrn = 26675-46-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4755415E-03 * exp(  -5341.347    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.4755415E-03 point
set label "" at    310.1500    ,   0.2377707E-03 point
set label "" at    298.1500    ,   0.4755415E-03 point ps 2 pt 6

plot [290:320] H(T)
