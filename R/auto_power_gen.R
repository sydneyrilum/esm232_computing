#' Automobile Power Generation
#'   
#' This function computes automobile fuel efficiency, by determining the power required to keep a car moving at a given speed
#' 
#' @param c_rolling rolling coefficient, default = 0.015
#' @param m vehicle mass (kg)
#' @param g acceleration due to gravity (m/s2), default = 9.8
#' @param V vehicle speed, assuming no headwind (m/s or mps)
#' @param A surface area of car (m2)
#' @param p_air density of air (kg/m3), default = 1.2
#' @param c_drag aerodynamic resistive coefficent, default = 0.3
#' @return Pb power (W)
#' 
# function definition
auto_power_gen = function(c_rolling = 0.015, m, g = 9.8, V, A, p_air = 1.2, c_drag = 0.3){
  result = (c_rolling * m * g * V) + (1/2 * A * p_air * c_drag * V^3)
  return(result)
}  