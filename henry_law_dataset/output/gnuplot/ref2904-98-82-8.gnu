# load "ref2904-98-82-8.gnu"
# chem = "1,2,3-trichlorobenzene"

set terminal postscript eps color
set title "ref = 2904; chem = 1,2,3-trichlorobenzene; casrn = 98-82-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1036680E-02 * exp(  -2201.172    *(1/   298.    -1/T))

set label "" at    298.5500    ,   0.9898157E-03 point
set label "" at    308.1500    ,   0.8357700E-03 point
set label "" at    318.1500    ,   0.6848490E-03 point
set label "" at    328.1500    ,   0.5250952E-03 point
set label "" at    333.1500    ,   0.4622479E-03 point
set label "" at    298.1500    ,   0.1036680E-02 point ps 2 pt 6

plot [290:340] H(T)
