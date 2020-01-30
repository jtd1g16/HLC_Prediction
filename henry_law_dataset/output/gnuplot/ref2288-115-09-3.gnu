# load "ref2288-115-09-3.gnu"
# chem = "chloromethylmercury"

set terminal postscript eps color
set title "ref = 2288; chem = chloromethylmercury; casrn = 115-09-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    21.62242     * exp(  -1813.674    *(1/   298.    -1/T))

set label "" at    298.1500    ,    21.81100     point
set label "" at    298.1500    ,    23.68616     point
set label "" at    298.1500    ,    19.24500     point
set label "" at    289.1500    ,    28.32470     point
set label "" at    289.1500    ,    28.32470     point
set label "" at    288.1500    ,    25.46262     point
set label "" at    288.1500    ,    29.01554     point
set label "" at    288.1500    ,    22.10708     point
set label "" at    288.1500    ,    26.94301     point
set label "" at    298.1500    ,    21.62242     point ps 2 pt 6

plot [280:300] H(T)
