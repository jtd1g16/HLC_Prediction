# load "ref3111-50-00-0.gnu"
# chem = "methanal"

set terminal postscript eps color
set title "ref = 3111; chem = methanal; casrn = 50-00-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    35.44505     * exp(  -5731.676    *(1/   298.    -1/T))

set label "" at    296.1500    ,    38.31318     point
set label "" at    313.1500    ,    15.87077     point
set label "" at    328.1500    ,    5.726819     point
set label "" at    298.1500    ,    35.44505     point ps 2 pt 6

plot [290:330] H(T)
