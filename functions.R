# Tb_gates default values
A = 1 # surface area (m^2)
D = 0.001 # characteristic dimension for conduction (m)
psa_dir	= 0.6 # proportion surface area exposed to sky (or enclosure)
psa_ref	= 0.4 # proportion surface area exposed to ground
psa_air = 0.6 # proportion surface area exposed to air
psa_g	= 0.2 # proportion surface area in contact with substrate
T_g	= 303 # ground surface temperature in K -- DATASET DEPENDENT 
T_a	= 310 # ambient air temperature in K -- DATASET DEPENDENT 
Qabs= 800 # Solar and thermal radiation absorbed (W) -- DATASET DEPENDENT 
epsilon	= 0.95 # longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
H_L	= 10 # Convective heat transfer coefficient (W m^-2 K^-1)
ef = 1.23 # enhancement factor
K = 0.15 # Thermal conductivity for insect cuticle (Galushko et al 2005) (W K^-1 m^-1)


Tb_Gates=function(A, D, psa_dir, psa_ref, psa_air, psa_g, T_g, T_a, Qabs, epsilon, H_L,ef=1.3, K){
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #Areas
  A_s = A*psa_dir 
  A_r = A*psa_ref 
  # Calculate skin area exposed to air
  A_air = A*psa_air
  # Calculate the area of contact
  A_contact  = A*psa_g
  
  #estimate effective radiant temperature of sky
  #Tsky=0.0552*(T_a)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*(T_a-273.15) -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
  #solve energy balance for steady state conditions
  # 0= Qabs -Qemit -Qconv -Qcond
  
  Qfn = function(Tb, Qabs, epsilon, sigma, A_s, Tsky, A_r, T_g, H_L, A_air, T_a, A_contact, K, D) {
    
    #Thermal radiaton emitted
    Qemit= epsilon*sigma*(A_s*(Tb^4 - Tsky^4)+A_r*(Tb^4 - T_g^4))
    #Convection
    Qconv= ef*H_L*A_air*(Tb-T_a)
    #Conduction
    Qcond= A_contact*K*(Tb-T_g)/D
    
    return(Qabs -Qemit -Qconv -Qcond)
  }
  
  
  Te <- tryCatch(uniroot(Qfn, c(273, 353),Qabs=Qabs, epsilon=epsilon, sigma=sigma, A_s=A_s, Tsky=Tsky, A_r=A_r, T_g=T_g, H_L=H_L, A_air=A_air, T_a=T_a, A_contact=A_contact, K=K, D=D, tol = 0.0001), error = function(e) {print("Unable to balance energy budget. One issue to check is whether absorbed solar radiation exceeds energy potentially lost to thermal radiation, convection, and conduction.")})
  Te.return=NA
  if(length(Te)>1) Te.return=Te$root
  
  return(Te.return)
}