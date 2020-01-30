# load "ref3118-174913-14-5.gnu"
# chem = "6-bromo-2,5-dichloroanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 6-bromo-2,5-dichloroanisole; casrn = 174913-14-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7663575E-02 * exp(  -3002.258    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.8489494E-02 point
set label "" at    318.1500    ,   0.4069286E-02 point
set label "" at    298.1500    ,   0.7663575E-02 point ps 2 pt 6

plot [290:320] H(T)
