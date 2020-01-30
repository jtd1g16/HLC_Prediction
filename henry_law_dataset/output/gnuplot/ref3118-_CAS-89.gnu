# load "ref3118-_CAS-89.gnu"
# chem = "2,6-dibromo-3-chloroanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 2,6-dibromo-3-chloroanisole; casrn = _CAS-89"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7350604E-02 * exp(  -770.3987    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.7546217E-02 point
set label "" at    318.1500    ,   0.6248540E-02 point
set label "" at    298.1500    ,   0.7350604E-02 point ps 2 pt 6

plot [290:320] H(T)
