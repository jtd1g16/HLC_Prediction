# load "ref1929-107-03-9.gnu"
# chem = "1-propanethiol"

set terminal postscript eps color
set title "ref = 1929; chem = 1-propanethiol; casrn = 107-03-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1692420E-02 * exp(  -3141.924    *(1/   298.    -1/T))

set label "" at    293.2000    ,   0.2315561E-02 point
set label "" at    303.1000    ,   0.1182519E-02 point
set label "" at    333.1000    ,   0.5893707E-03 point
set label "" at    298.1500    ,   0.1692420E-02 point ps 2 pt 6

plot [290:340] H(T)
