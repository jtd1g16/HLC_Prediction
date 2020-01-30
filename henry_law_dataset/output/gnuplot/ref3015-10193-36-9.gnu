# load "ref3015-10193-36-9.gnu"
# chem = "silicic acid"

set terminal postscript eps color
set title "ref = 3015; chem = silicic acid; casrn = 10193-36-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2319387E+11 * exp(  -13764.41    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1934317E+13 point
set label "" at    298.1500    ,   0.2417960E+11 point
set label "" at    323.1500    ,   0.5984031E+09 point
set label "" at    348.1500    ,   0.2605643E+08 point
set label "" at    373.1500    ,    1793672.     point
set label "" at    398.1500    ,    179292.7     point
set label "" at    423.1500    ,    24313.21     point
set label "" at    448.1500    ,    4241.937     point
set label "" at    473.1500    ,    911.2117     point
set label "" at    498.1500    ,    232.2409     point
set label "" at    523.1500    ,    68.22255     point
set label "" at    548.1500    ,    22.46088     point
set label "" at    573.1500    ,    8.042783     point
set label "" at    598.1500    ,    3.018546     point
set label "" at    623.1500    ,    1.101613     point
set label "" at    628.1500    ,   0.8805357     point
set label "" at    633.1500    ,   0.6898888     point
set label "" at    638.1500    ,   0.5224506     point
set label "" at    643.1500    ,   0.3685331     point
set label "" at    644.1500    ,   0.3358049     point
set label "" at    645.1500    ,   0.2996246     point
set label "" at    646.1500    ,   0.2560904     point
set label "" at    647.0960    ,   0.1587817     point
set label "" at    298.1500    ,   0.2319387E+11 point ps 2 pt 6

plot [270:650] H(T)
