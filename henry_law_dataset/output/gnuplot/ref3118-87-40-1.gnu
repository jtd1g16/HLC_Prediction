# load "ref3118-87-40-1.gnu"
# chem = "2,4,6-trichloroanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 2,4,6-trichloroanisole; casrn = 87-40-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4430414E-02 * exp(  -637.3376    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.4527730E-02 point
set label "" at    318.1500    ,   0.3873326E-02 point
set label "" at    298.1500    ,   0.4430414E-02 point ps 2 pt 6

plot [290:320] H(T)
