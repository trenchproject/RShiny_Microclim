grabAnyData <- function(dataset, inputVar, loc, month) {
  if (dataset == "SCAN") {
    data <- grabSCAN(inputVar, loc, month)
  } else if (dataset == "ERA5") {
    data <- grabERA(inputVar, loc, month)
  } else if (dataset == "GLDAS") {
    data <- grabGLDAS(inputVar, loc, month)
  } else if (dataset == "GRIDMET") {
    data <- grabGRID(inputVar, loc, month)
  } else if (dataset == "NOAA_NCDC") {
    data <- grabNOAA(inputVar, loc, month)
  } else if (dataset == "microclimUS") {
    data <- grabmicroUS(inputVar, loc, month)
  } else if (dataset == "microclim") {
    data <- grabmicro(inputVar, loc, month)
  } else if (dataset == "USCRN") {
    data <- grabUSCRN(inputVar, loc, month)
  } else if (dataset == "SNODAS") {
    data <- grabSNODAS(inputVar, loc, month)
  } else if (dataset == "micro_ncep") {
    data <- grabMicroNCEP(inputVar, loc, month)
  } else if (dataset == "micro_usa") {
    data <- grabMicroUSA(inputVar, loc, month)
  } else if (dataset == "micro_global") {
    data <- grabMicroGlobal(inputVar, loc, month)
  } else if (dataset == "NCEP") {
    data <- grabNCEP(inputVar, loc, month)
  } else if (dataset == "NEW01") {
    data <- grabNEW01(inputVar, loc, month)
  }
  return (data)
}

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



Tb_lizard=function(T_a, T_g, u, svl, m, psi, rho_S, elev, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5){
  
  psi= psi*pi/180 #convert zenith angle to radians
  
  # constants
  sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
  c_p=29.3 # specific heat of air, J/mol °C (p.279) Parentheses all from Campbell & Norman 1998
  
  tau=0.65 # atmospheric transmisivity
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p.159)
  
  # Calculate radiation
  # view angles, parameterize for animal suspended above ground (p181), on ground- adjust F_e, F_r, and F_g
  h=svl/1000 # length of svl in m
  
  A=0.121*m^0.688   # total lizard area, Roughgarden (1981)
  A_p= (-1.1756810^-4*psi^2-9.2594*10^-2*psi+26.2409)*A/100      # projected area
  F_p=A_p/A
  
  # radiation
  p_a=101.3* exp (-elev/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos (psi))  # (11.12) optical air mass
  m_a[(psi>(80*pi/180))]=5.66
  
  # Flux densities
  epsilon_ac= 9.2*10^-6*(T_a+273)^2 # (10.11) clear sky emissivity
  L_a=sigma*(T_a+273)^4  # (10.7) long wave flux densities from atmosphere 
  L_g=sigma*(T_g+273)^4  # (10.7) long wave flux densities from ground
  
  S_d=0.3*(1-tau^m_a)* S_p0 * cos(psi)  # (11.13) diffuse radiation
  
  dd2= 1+2*0.1675*cos(2*pi*doy/365)
  S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  S_b = S_p * cos(psi)
  S_t = S_b + S_d
  S_r= rho_S*S_t # (11.10) reflected radiation
  
  #__________________________________________________
  # conductance
  
  dim=svl/1000 # characteristic dimension in meters
  g_r= 4*epsilon_s*sigma*(T_a+273)^3/c_p # (12.7) radiative conductance
  
  g_Ha=1.4*0.135*sqrt(u/dim) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)
  
  #__________________________________________________
  # operative environmental temperature
  
  #calculate with both surface and air temp (on ground and in tree)
  
  sprop=1 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  Te=T_a+(R_abs-epsilon_s*sigma*(T_a+273)^4)/(c_p*(g_r+g_Ha))         # (12.19) Operative temperature            
  Te_surf= T_g+(R_abs-epsilon_s*sigma*(T_g+273)^4)/(c_p*(g_r+g_Ha))        
  
  # calculate in shade, no direct radiation
  sprop=0 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  TeS=T_a+(R_abs-epsilon_s*sigma*(T_a+273)^4)/(c_p*(g_r+g_Ha))         # (12.19) Operative temperature                        
  TeS_surf=T_g+(R_abs-epsilon_s*sigma*(T_g+273)^4)/(c_p*(g_r+g_Ha))  
  
  #Select Te to return
  if(sun==TRUE & surface==TRUE) Te= Te_surf
  if(sun==TRUE & surface==FALSE) Te= Te
  if(sun==FALSE & surface==TRUE) Te= TeS_surf
  if(sun==FALSE & surface==FALSE) Te= TeS
  
  return(Te) 
}


Tb_CampbellNorman=function(T_a, T_g, S, alpha_L=0.96, epsilon=0.96, c_p=29.3, D, V){
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #solar and thermal radiation absorbed
  L_a=sigma*T_a^4  # (10.7) long wave flux densities from atmosphere 
  L_g=sigma*T_g^4  # (10.7) long wave flux densities from ground
  F_a=0.5; F_g=0.5 #proportion of organism exposure to air and ground, respectively
  R_abs= S+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  
  #thermal radiation emitted
  Qemit= epsilon*sigma*T_a^4
  
  #conductance
  g_Ha=1.4*0.135*sqrt(V/D) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976), assumes forced conduction
  g_r= 4*epsilon*sigma*T_a^3/c_p # (12.7) radiative conductance
  
  # operative environmental temperature
  T_e=T_a+(R_abs-Qemit)/(c_p*(g_r+g_Ha))                       
  
  return(T_e) 
}

day_of_year<- function(day, format="%Y-%m-%d"){
  day=  as.POSIXlt(day, format=format)
  return(as.numeric(strftime(day, format = "%j")))
}

Qmetabolism_from_mass_temp<-function(m, T_b, taxa){
  
  stopifnot(m>0, T_b>200, T_b<400, taxa %in% c("bird","mammal","reptile","amphibian","invertebrate") )
  
  #Source:  Gillooly JF et al. 2001. Effects of size and temperature on metabolic rate. Science 293: 2248-2251. 
  if(taxa=="bird" | taxa=="mammal") Qmet= exp(-9100/T_b+29.49)*m^0.75/60
  if(taxa=="reptile") Qmet= exp(-8780/T_b+26.85)*m^0.75/60
  if(taxa=="amphibian") Qmet= exp(-5760/T_b+16.68)*m^0.75/60
  if(taxa=="invertebrate") Qmet= exp(-9150/T_b+27.62)*m^0.75/60
  return(Qmet)
}
