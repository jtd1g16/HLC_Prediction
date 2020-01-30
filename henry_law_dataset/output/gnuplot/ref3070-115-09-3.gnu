# load "ref3070-115-09-3.gnu"
# chem = "chloromethylmercury"

set terminal postscript eps color
set title "ref = 3070; chem = chloromethylmercury; casrn = 115-09-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    19.78208     * exp(  -4138.025    *(1/   298.    -1/T))

set label "" at    283.1500    ,    47.19617     point
set label "" at    288.1500    ,    26.08719     point
set label "" at    298.1500    ,    21.23134     point
set label "" at    298.1500    ,    19.78208     point ps 2 pt 6

plot [280:300] H(T)
