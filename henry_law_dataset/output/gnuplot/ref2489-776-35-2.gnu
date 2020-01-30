# load "ref2489-776-35-2.gnu"
# chem = "9,10-dihydrophenanthrene"

set terminal postscript eps color
set title "ref = 2489; chem = 9,10-dihydrophenanthrene; casrn = 776-35-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1155479     * exp(  -7464.625    *(1/   298.    -1/T))

set label "" at    277.9600    ,   0.7092199     point
set label "" at    279.1500    ,   0.6329114     point
set label "" at    283.8200    ,   0.4048583     point
set label "" at    288.0500    ,   0.2793296     point
set label "" at    293.1400    ,   0.1865672     point
set label "" at    298.1600    ,   0.1111111     point
set label "" at    298.1500    ,   0.1155479     point ps 2 pt 6

plot [270:300] H(T)
