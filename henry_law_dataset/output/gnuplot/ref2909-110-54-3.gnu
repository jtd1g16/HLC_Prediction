# load "ref2909-110-54-3.gnu"
# chem = "hexane"

set terminal postscript eps color
set title "ref = 2909; chem = hexane; casrn = 110-54-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5875978E-05 * exp(  -4009.764    *(1/   298.    -1/T))

set label "" at    287.6700    ,   0.9657912E-05 point
set label "" at    287.6700    ,   0.9699721E-05 point
set label "" at    293.2000    ,   0.7383706E-05 point
set label "" at    293.2000    ,   0.7342685E-05 point
set label "" at    298.1500    ,   0.5728216E-05 point
set label "" at    298.1500    ,   0.5808895E-05 point
set label "" at    303.3500    ,   0.4638822E-05 point
set label "" at    303.3500    ,   0.4718118E-05 point
set label "" at    308.0500    ,   0.3787184E-05 point
set label "" at    308.0500    ,   0.3904313E-05 point
set label "" at    298.1500    ,   0.5875978E-05 point ps 2 pt 6

plot [280:310] H(T)
