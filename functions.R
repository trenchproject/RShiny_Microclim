grabAnyData <- function(dataset, inputVar, loc, month) {
  if (dataset == "SCAN") {
    data <- grabSCAN(inputVar, loc, month)
  } else if (dataset == "ERA5") {
    data <- grabERA(inputVar, loc, month)
  } else if (dataset == "ERA51cm") {
    data <- grabERA1cm(inputVar, loc, month)
  }else if (dataset == "GLDAS") {
    data <- grabGLDAS(inputVar, loc, month)
  } else if (dataset == "GLDAS1cm") {
    data <- grabGLDAS1cm(inputVar, loc, month)
  }else if (dataset == "GRIDMET") {
    data <- grabGRID(inputVar, loc, month)
  } else if (dataset == "NOAA_NCDC") {
    data <- grabNOAA(inputVar, loc, month)
  } else if (dataset == "microclimUS") {
    data <- grabmicroUS(inputVar, loc, month)
  } else if (dataset == "microclim") {
    data <- grabmicro(inputVar, loc, month)
  } else if (dataset == "USCRN") {
    data <- grabUSCRN(inputVar, loc, month)
  } else if (dataset == "USCRN1cm") {
    data <- grabUSCRN1cm(inputVar, loc, month)
  }else if (dataset == "SNODAS") {
    data <- grabSNODAS(inputVar, loc, month)
  } else if (dataset == "micro_ncep") {
    data <- grabMicroNCEP(inputVar, loc, month)
  } else if (dataset == "micro_usa") {
    data <- grabMicroUSA(inputVar, loc, month)
  } else if (dataset == "micro_global") {
    data <- grabMicroGlobal(inputVar, loc, month)
  } else if (dataset == "micro_era5") {
    data <- grabMicroERA5(inputVar, loc, month)
  } else if (dataset == "NCEP") {
    data <- grabNCEP(inputVar, loc, month)
  } else if (dataset == "NCEP1cm") {
    data <- grabNCEP1cm(inputVar, loc, month)
  }else if (dataset == "NEW01") {
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

air_temp_profile_neutral<-function(T_r, zr, z0, z, T_s){
  
  stopifnot(zr>=0, z0>=0, z>=0)
  
  T_z= (T_r-T_s)*log(z/z0+1)/log(zr/z0+1)+T_s 
  return(T_z)
}

heat_transfer_coefficient_approximation<-function(V, D, K, nu, taxa="sphere"){
  
  stopifnot(V>=0, D>=0, K>=0, nu>=0, taxa %in% c("sphere","frog","lizard","flyinginsect","spider"))
  
  taxas= c("sphere","frog","lizard","flyinginsect","spider")
  
  # Dimensionless constant (Cl)
  Cls= c(0.34,0.196,0.56,0.0714,0.52)
  ns= c(0.6,0.667,0.6, 0.78,0.5) 
  
  #find index  
  ind= match(taxa, taxas)
  
  Re= V*D/nu #Reynolds number 
  Nu <- Cls[ind] * Re^ns[ind]  #Nusselt number
  H_L= Nu * K / D
  
  return(H_L)
}

sa_from_mass <- function(m, taxa){
  
  stopifnot(taxa %in% c("lizard", "salamander", "frog", "insect"), m > 0)
  
  if (taxa == "lizard") {
    
    # initial mass in kg
    
    0.0314 * pi * (m / 1000) ^ (2 / 3)
    
  } else if (taxa == "salamander") {
    
    # convert cm^2 to m^2  
    
    8.42 * m ^ 0.694 / (100 * 100) 
    
  } else if (taxa == "frog") {
    
    9.9 * m ^ 0.56 * (0.01) ^ 2 
    
  } else if (taxa == "insect" ) {
    
    0.0013 * m ^ 0.8 
    
  } 
  
}

volume_from_length <- function(l, taxa) {
  
  stopifnot(taxa %in% c("lizard", "frog", "sphere"), l > 0)
  
  Kl <- switch(taxa, 
               "lizard" = 3.3,
               "frog" = 2.27,
               "sphere" = 1.24)
  
  
  (l / Kl) ^ 3
  
}

partition_solar_radiation=function(method, kt, lat=NA, sol.elev=NA){  
  
  stopifnot(method %in% c("Liu_Jordan", "Orgill_Hollands", "Erbs", "Olyphant", "Spencer", "Reindl-1", "Reindl-2", "Lam_Li"), kt>=0, kt<=1)
  
  # Methods from Wong and Chow (2001, Applied Energy 69:1991-224)
  
  #based on the correlations between the clearness index kt (dimensionless) and the diffuse fraction kd (dimensionless), diffuse coefficient kD (dimensionless) or the direct transmittance kb (dimensionless) where
  #k_t= I_t/I_o, k_d=I_d/I_t, k_D=I_d/I_o, k_b=I_b/I_o,
  #where I_t, I_b, I_d, and I_o are the global, direct, diffuse, and extraterrestial irradiances, respectively
  
  #kd- diffuse fraction
  
  #6.1 Liu and Jordan 
  if(method=="Liu_Jordan") {
    kd= (0.271 -0.294*kt)/kt #kd= (0.384 -0.416*kt)/kt
    if(kd>1) kd=1
  }
  
  #6.2 Orgill and Hollands
  if(method=="Orgill_Hollands"){
    if(kt<0.35) kd= 1-0.249*kt
    if(kt>=0.35 & kt<=0.75) kd= 1.577-1.84*kt
    if(kt>=0.75) kd = 0.177 
  }
  
  #6.3 Erbs et al.
  if(method=="Erbs"){
    if(kt<=0.22) kd= 1-0.09*kt
    if(kt>0.22 & kt<0.8) kd= 0.9511 -0.1604*kt +4.388*kt^2 -16.638*kt^3 +12.336*kt^4
    if(kt>=0.8) kd = 0.165 #Correction from 0.125 for CO from Olyphant 1984
  }
  
  if(method=="Olyphant"){ #Correction for Colorado from Olyphant 1984
    if(kt<=0.22) kd= 1-0.09*kt
    if(kt>0.22 & kt<0.8) kd= 0.9511 -0.1604*kt +4.388*kt^2 -16.638*kt^3 +12.336*kt^4
    if(kt>=0.8) kd = 0.125 
  }
  
  #6.4 Spencer
  if(method=="Spencer"){
    a3= 0.94+0.0118*abs(lat)
    b3= 1.185+0.0135*abs(lat)
    
    #method assumes constant kd if kt outside below range
    kd=NA
    if(kt>=0.35 & kt<=0.75) kd= a3-b3*kt
  }
  
  #6.5 Reindl et al.
  if(method=="Reindl-1"){
    if(kt<=0.3) kd= 1.02-0.248*kt
    if(kt>0.3 & kt<0.78) kd= 1.45-1.67*kt
    if(kt>=0.78) kd = 0.147
  }
  
  if(method=="Reindl-2"){
    if(kt<=0.3) kd= 1.02-0.254*kt
    if(kt>0.3 & kt<0.78) kd= 1.4-1.749*kt+0.177*sin(sol.elev*180/pi)
    if(kt>=0.78) kd = 0.486*kt -0.182*sin(sol.elev*180/pi)
  }
  
  #6.6 Lam and Li
  if(method=="Lam_Li"){
    if(kt<=0.15) kd= 0.977
    if(kt>0.15 & kt<=0.7) kd= 1.237-1.361*kt
    if(kt>0.7) kd = 0.273
  }
  
  #direct and diffuse is c(rad*(1-kd),rad*(kd))
  
  return (kd)
  
}  


zenith_angle=function(doy, lat, lon, hour, offset=NA){
  
  stopifnot(doy>0, doy<367, lat>=-90, lat<=90, lon>=-180, lon<=180, hour>=0, hour<=24)
  
  lat=lat*pi/180 #to radians
  
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));                            # Declination angle in radians           
  
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f=f*pi/180 #convert f in degrees to radians
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  lon[lon<0]=360+lon[lon<0] #convert to 0 to 360
  LC= 1/15*(lon%%15) # longitude correction, 1/15h for each degree of standard meridian
  LC[LC>0.5]= LC[LC>0.5]-1
  t_0 = 12-LC-ET # solar noon
  
  #Check if offset is as expected. (Is the timezone of the location the same as that of the meridian 
  #that's within 7.5 degrees from that location?)
  lon[lon>180]=lon[lon>180]-360
  if (!is.na(offset)) {
    offset_theory <- as.integer(lon / 15) + lon / abs(lon) * as.integer(abs(lon) %% 15 / 7.5)
    t_0 = t_0 - offset_theory + offset
  }
  
  cos.zenith= sin(DecAng)*sin(lat) + cos(DecAng)*cos(lat)*cos(pi/12*(hour-t_0)); #cos of zenith angle in radians
  zenith=acos(cos.zenith)*180/pi # zenith angle in degrees
  zenith[zenith>90]=90 # if measured from the vertical psi can't be greater than pi/2 (90 degrees)
  
  return(zenith)
}
