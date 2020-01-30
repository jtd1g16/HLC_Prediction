# load "ref3118-_CAS-88.gnu"
# chem = "3-bromo-2,6-dichloroanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 3-bromo-2,6-dichloroanisole; casrn = _CAS-88"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1094394E-01 * exp(  -2665.892    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.1198517E-01 point
set label "" at    318.1500    ,   0.6238229E-02 point
set label "" at    298.1500    ,   0.1094394E-01 point ps 2 pt 6

plot [290:320] H(T)
