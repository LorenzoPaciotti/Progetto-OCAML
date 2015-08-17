type seg_pol = {x:float; y:float; r:float; a:float};;
let pi = 3.1415927 ;;
let s = {x=100.; y= 0.; a= pi /. 2.; r = 100.};;
draw_r s 6 [ (-.pi/.2.),0.6,1.; (pi/.2.), 0.6,1.0] ;;
Graphics.clear graph () ;;
draw_r s 6 [(-.pi /. 6.), 0.6, 0.766;
(-.pi /. 4.), 0.55, 0.333;
(pi /. 3.), 0.4, 0.5 ] ;;
