# load "ref0727-374-01-6.gnu"
# chem = "1,1,1-trifluoro-2-propanol"

set terminal postscript eps color
set title "ref = 727; chem = 1,1,1-trifluoro-2-propanol; casrn = 374-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4533294     * exp(  -6291.082    *(1/   298.    -1/T))

set label "" at    288.0000    ,   0.9719046     point
set label "" at    298.0000    ,   0.4487205     point
set label "" at    308.0000    ,   0.2267729     point
set label "" at    318.0000    ,   0.1238678     point
set label "" at    298.1500    ,   0.4533294     point ps 2 pt 6

plot [280:320] H(T)
