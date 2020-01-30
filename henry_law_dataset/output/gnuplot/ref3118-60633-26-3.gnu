# load "ref3118-60633-26-3.gnu"
# chem = "6-bromo-2,4-dichloroanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 6-bromo-2,4-dichloroanisole; casrn = 60633-26-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8173545E-02 * exp(  -3136.271    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.9095887E-02 point
set label "" at    318.1500    ,   0.4219159E-02 point
set label "" at    298.1500    ,   0.8173545E-02 point ps 2 pt 6

plot [290:320] H(T)
