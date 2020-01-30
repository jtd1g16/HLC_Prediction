# load "ref0287-10599-90-3.gnu"
# chem = "chloramide"

set terminal postscript eps color
set title "ref = 287; chem = chloramide; casrn = 10599-90-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9223919     * exp(  -4798.234    *(1/   298.    -1/T))

set label "" at    293.1500    ,    1.213738     point
set label "" at    313.1500    ,   0.4267048     point
set label "" at    298.1500    ,   0.9223919     point ps 2 pt 6

plot [290:320] H(T)
