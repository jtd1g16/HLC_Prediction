# load "ref3118-50375-10-5.gnu"
# chem = "2,3,6-trichloroanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 2,3,6-trichloroanisole; casrn = 50375-10-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1060458E-01 * exp(  -4465.558    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.1234836E-01 point
set label "" at    318.1500    ,   0.4136068E-02 point
set label "" at    298.1500    ,   0.1060458E-01 point ps 2 pt 6

plot [290:320] H(T)
