# load "ref3118-95970-19-7.gnu"
# chem = "2,3,6-tribromoanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 2,3,6-tribromoanisole; casrn = 95970-19-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5216705E-02 * exp(  -2800.846    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.5739376E-02 point
set label "" at    318.1500    ,   0.2890189E-02 point
set label "" at    298.1500    ,   0.5216705E-02 point ps 2 pt 6

plot [290:320] H(T)
