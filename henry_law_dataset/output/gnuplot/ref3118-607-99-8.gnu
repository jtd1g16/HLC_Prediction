# load "ref3118-607-99-8.gnu"
# chem = "2,4,6-tribromoanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 2,4,6-tribromoanisole; casrn = 607-99-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1924422E-01 * exp(  -6441.711    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.2397034E-01 point
set label "" at    318.1500    ,   0.4948124E-02 point
set label "" at    298.1500    ,   0.1924422E-01 point ps 2 pt 6

plot [290:320] H(T)
