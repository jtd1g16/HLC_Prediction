# load "ref2895-106-93-4.gnu"
# chem = "1,2-dibromoethane"

set terminal postscript eps color
set title "ref = 2895; chem = 1,2-dibromoethane; casrn = 106-93-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1086636E-01 * exp(  -2974.577    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1246492E-01 point
set label "" at    308.1500    ,   0.6205843E-02 point
set label "" at    323.1500    ,   0.5545592E-02 point
set label "" at    298.1500    ,   0.1086636E-01 point ps 2 pt 6

plot [290:330] H(T)
