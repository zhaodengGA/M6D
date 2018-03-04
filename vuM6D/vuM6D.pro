;
;CODE NAME:
;   vuM6D
;
;
; AUTHOR:
;  ZHAO Deng
;  email: zhao.deng@foxmail.com
;
;
; COAUTHOR: 
;   R. E. Waltz
;   
;   
; PURPOSE: 
;  To plot the data from M6D code, and analyze the physics.
;   
;   
; HOW TO START:
; 1. Make sure M6D output files needed for plotting and all the vuM6D '.pro' files are in the same directory.
; 2. Compile cgplot first.
; 3. Compile vuM6D
; 4. Run vuM6D
; 
; 
; CODE STRUCTURE:
; Main plots:       To plot the basic information of a case, including: density, gamma_r, omega, den_mode, phi_mode vs time;
;                   den_mode vs theta, den_mode vs r.
; 
; Colorful plot:    To plot the colorful contour figures of growth rate and frequency, 
;                   
;                    
; ColorMap:         To plot the color table for the Colorful plot
; 
;
;; INPUT FILES: 
; M6D.input   -- in order to read the grid parameters and physics variables.
; vuM6D.input -- in order to read the control parameters for IDL plotting.
; 
; 
; CONTROL VARIABLES INTRODUCE:
; The variables in vuM6D.input is used for control the idl plot.
; 
; avenum=4             The number of pieces for taking average of energy overplot.          
; plotstep=1           The interval of kx lines in Omega overplot.
; sattime=0.0          For setting the starting point of time average.
; Intermit_Plot=0      A switch of output intermittency and average value. 
;                      1 for output intermit&average ON, 0 for OFF 
; overplot_log=0       A switch between normal axis and logarithmic axis in 3D plot. 
;                      0 for normal, 1 for ylog, 2 for xlog and ylog
; Az=-60               To rotate a certain angle along z axis in 3D plot
; Ax=35                To rotate a certain angle along x axis in 3D plot
; ix1=0                If ix1=0 & ix2=0, take the time=0 moment data for Calculate botton.
; ix2=0                 
; Switch_sat=1         A switch of outputting the ave_time given by saturated time in colorfulplot. 
;                      1 for ON, 0 for OFF.  
; Omega_min=0.01       The minimum value of y axis. 0 for xmin=min(x), >0 for xmin=Omega_min
; Omega_max=60         The maximum value of y axis.  0 for xmax=max(x), >0 for xmax=Omega_max
; Phi_logmin=0.0001    The minimum value of z axis for 3D log plot of |Phi|
; Chi_logmin=0.000001  The minimum value of z axis for 3D log plot of |Chi|
; OMGpoint=10          The frequency region chosen to plot high frequency ion cyclotron (IC) modes in Omega vs ky plot.  
; freqPer=0.2          The pick up frequency range for choosing IC modes. 
;                      e.g. freqPer=0.2 means if the frequency of modes are 20% smaller or larger than OMGpoint, 
;                      it will be chosen as the first harmonic of IC mode. 
; begintime=0          The begin time of eddy movie show.
;
; 
; BUTTON INTRODUCE:
; *total Chi (GK) (to plot total Chi vs time) window:
; Plot:                 Start plot with the default parameters.
; t+ or t-:             To adjust the point for taking the average of a saturation period.
; aveT and aveP:        Used for take the average of two state situation.
;                       aveT is the switch between take the average of one state or two states.
;                       aveP is change the current point to be adjusted by t+ or t-.
; ZOOM in or ZOOM out:  To adjust the range of the y axis.
; Yrange:               To decide the way of adjusting the yrange. Such as: fix the top, middle or bottom part.
; log:                  A switch between normal axis and logarithmic axis.
; PDF:                  To show the plot of the particle distribution function
; Calculate:            TO calculate anything plugged in there.
; Done:                 Close this window.
;      
; *Omega (GK) (to plot  growth rate (and frequency) vs ky) window:
; Plot(re):             To plot the real part of Omega, which means frequency.
; Plot(im):             To plot the imaginary part of Omega, which means growth rate.
; MS/Init:              To switch between plotting Matrix Solver eigenvalue method result and plotting the time initial
;                       value method result.
; 3D:                   To show the 3D plot
; 
; *F_mu (to plot delta f vs mu and time) window:
; Z+ or Z-:             To rotate a certain angle along z axis.           
; X+ or X-:             To rotate a certain angle along x axis.     
; zmin*10 or zmin/10:   To change the minimum value of zrange.
; jump*2 or jump/2:     To change the interval of sampling.   
; 2D:                   To show the 2D plot
; 
; *Omega_matr (to plot the colorful contour figures of growth rate and frequency) window:
; Type:                 To switch between GK, CKinCH and CKinFH
; 
; *eddy movie (to plot the eddy in (x,y) space evolution in time) window:
; velocity:             To show the velocity vector plot.
;    
;
; 
; REVISE TIME & CONTENT:
; 2016-10-17: add annotation

;*****************************************************************************************************************

pro twoD_time_trace_see,nametemp,windownum,group=group

;  common GLOBAL 

  ;;-----------------------------------------------
  ;; Private (local) data
  ;;
;  common PRIVATE_time_trace,widget
  common plotaxis,zoomz,zoombackup0,zoombackup1,log,half,freq,i_ptype,satdelta_t,name,yrange_style
  common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark

  ;;-----------------------------------------------

  ;;------------------------------------------
  ;; Return conditions
  ;;
 ; if exists_diff eq 0 then return
 ; if xregistered('diffusion_ave_see') then return
  ;;------------------------------------------
  name=nametemp
  base = widget_base(title=name[0],$
                     /column)
   ;;set default value

  defsysv,'!aveT',0 ;; ave TYPE
  defsysv,'!aveP',0 ;; ave POINT
  defsysv,'!tminus',0
  defsysv,'!tplus',0
  defsysv,'!tflag',0  ;; choose point  
  defsysv,'!point1',.0
  defsysv,'!point2',.0
  defsysv,'!point3',.0
  defsysv,'!point4',.0
  defsysv,'!Im_Re',0  ;;!Im_Re--to mark the Im or Re plot of complex number plot  

  i_ptype=0
  log=0
  zoomz = 1.0  ;zoomz is the parameter which used to adjust the range of the axis
  satdelta_t=0.0  ;satdelta_t is the parameter which used to adjust the saturation time when the user click the button "t+" or "t-"
  yrange_style=1  ;yrange_style is the parameter which used to choose plot the top, middle or bottom part of the picture.
                  ;for the value of yrange_style/3, 0 for top, 1 for middle, 2 for bottom;
  zoombackup0=1.0  ;zoombackup0 and zoombackup1 are used to set zoomz=1, when changes the plot between plot(re) and plot(im).
  zoombackup1=1.0
                    
  ;i_tp = 0
  ;;----------------------------------------------------------
  ;; BUTTONS
  ;;----------------------------------------------------------

  row1 = widget_base(base,$
                     /row,$
                     /frame)


 IF(strcmp(name[3],'complex',7) ) THEN BEGIN
  x = widget_button(row1, $
                    value='Plot(re)', $
                    uvalue=0)

  x = widget_button(row1, $
                    value='Plot(im)', $
                    uvalue=1)
 Endif Else Begin
  x = widget_button(row1, $
                    value='Plot', $
                    uvalue=0)
 Endelse
 
 

  x = widget_button(row1, $
                    value='t+', $
                    uvalue=2)

  x = widget_button(row1, $
                    value='t-', $
                    uvalue=3)
                    
   x = widget_button(row1, $
                    value='t+ +', $
                    uvalue=4)

  x = widget_button(row1, $
                    value='t- -', $
                    uvalue=5)                                    

  x = widget_button(row1, $
                    value='point', $
                    uvalue=6)

  x = widget_button(row1, $
                    value='ZOOM in', $
                    uvalue=7)

  x = widget_button(row1, $
                    value='ZOOM out', $
                    uvalue=8)
  x = widget_button(row1, $
                    value='Yrange', $
                    uvalue=9)
;  x = widget_button(row1, $
;                    value='units', $
;                    uvalue=8)

  x = widget_button(row1,$
                    value='log',$
                    uvalue=10)

;  x = widget_button(row1,$
;                    value='TYPE',$
;                    /menu)
;
;  tlevels=['Line Plot','PDF']
;  for i=0,1 do begin
;     x1 = widget_button(x,$
;                        value=tlevels[i],$
;                        uvalue=20+i)
;  endfor
;
;  x = widget_button(row1, $
;                    value='Calculate', $
;                    uvalue=10)

  x = widget_button(row1, $
                    value='Done', $
                    uvalue=15)

  ;;----------------------------------------------------------
  ;; DRAW WIDGET and CONTROL
  ;;----------------------------------------------------------

  draw = widget_draw(base,     $
                     xsize=700, $
                     ysize=500)

  widget_control, base, $
    ;set_uvalue=state,$
    /no_copy, $
    /realize

  ;!plotkspecid=!D.WINDOW
    (*windowmark)[windownum]=!D.window
    basemark[windownum]=base


  xmanager,'twoD_time_trace', $
    base,$
    group_leader=group


end

;*******************************************************************************
pro twoD_time_trace_event,event

  common startup,number_plot,fpath,ncolor,color_value,plotid
  common plotaxis,zoomz,zoombackup0,zoombackup1,log,half,freq,i_ptype,satdelta_t,name,yrange_style
  common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark

  widget_control, event.id, $
    get_uvalue=uvalue
 ; wset, widget

cd,!workFolder  ;go to the working directory
openr,lun,'M6D.input',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_parameter',plot_name_temp,19)
  endwhile
  thisFormat='(8x,i2,/,7x,i9,/,8x,i2,/,9x,i2)'
  readf,lun,restart,n_data,IC_flag,i_solver,Format=thisFormat
  
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Grid_parameter',plot_name_temp,16)
  endwhile
  thisFormat='(11x,i15,/,6x,f12.8,/,4x,i5,/,4x,i5,/,8x,i5,/,4x,i5,/,6x,i5,/,6x,i5,/,5x,i5,/,5x,i5)'
  readf,lun,n_step_max,del_t,n_g,n_r,n_theta,n_n,n_min,n_del,n_v1,n_v3,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Physics_parameter',plot_name_temp,19)
  endwhile
  thisFormat='(11x,f7.4,/,8x,f7.4,/,8x,f7.4,/,10x,f7.4,/,6x,f5.3,/,6x,f8.3,/,5x,f12.7,/,5x,f12.7,/,7x,f12.7,/,6x,f12.7,/,6x,f8.4,/,8x,f12.7)'
  readf,lun,Omega_star,r_hat_a,r_hat_b,Rmaj0_hat,q_mid,s_hat,aoLn,aoLT,nu_mid,v_max,delta,delta_i, Format=thisFormat

free_lun,lun


cd,!vuM6DFolder
openr,lun,'vuM6D.input',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_variables',plot_name_temp,19)
  endwhile
  thisFormat='(7x,i5,/,9x,i5,/,8x,f10.3,/,14x,i2,/,13x,i2,/,/,/,4x,i6,/,4x,i6,/,11x,i4,/,13x,i2,/,5x,i5,/,8x,i2,/,2x,i4,/,2x,i4)'
  readf,lun,avenum,plotstep,sattime,Intermit_Plot,overplot_log,ix1,ix2,Switch_sat,quicktrigger,jump,control,k_theta,p_r,Format=thisFormat
  
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Range_control',plot_name_temp,15)
  endwhile
  thisFormat='(10x,f15.12,/,10x,f12.4,/,11x,f15.10,/,11x,f15.12,/,9x,i5,/,8x,f10.4,/,10x,f12.3)'
  readf,lun,Omega_min,Omega_max,Phi_logmin,Chi_logmin,OMGpoint,freqPer,begintime,Format=thisFormat

free_lun,lun

cd,!workFolder

   timetemp1=0.0
   timetemp2=0.0
   openr,lun,'time.out',/get_lun 
   readf,lun,timetemp1
   readf,lun,timetemp1
   POINT_LUN, -lun, pos1
   readf,lun,timetemp2
   POINT_LUN, -lun, pos2
   free_lun,lun  
   
   openr,lun,'time.out',/get_lun,/APPEND 
   POINT_LUN, -lun, posEnd
   POINT_LUN, lun, posEnd-(pos2-pos1)
   readf,lun,Endtime
   print,'Endtime=',Endtime
   free_lun,lun

   n_step_max=round(Endtime/del_t)
   
   Time=fltarr(n_step_max/n_data+1)
   openr,lun,'time.out',/get_lun
   readf,lun,time
   free_lun,lun  
   

  ;set the initial value of the four average points

  ;;-------------------------------------------------------
  ;; MENU
  ;;-------------------------------------------------------

  ;;set the click botton action act on the window you want.
  For i=0,200-1 Do begin
  IF(basemark[i] eq event.top)then begin
    IF((*windowmark)[i] ne !D.window)then begin
    wset,(*windowmark)[i]
    Endif
    case(i) of
    0: name=['density','density','den.out']
    1: name=['total D (GK)','D','total_D_3G.txt']
    2: name=['total Entropy (GK)','entropy','total_entropy.txt']
    3: name=['total '+'$\chi$$\downi$'+' (GK)','$\chi$$\downi$','total_Chi.txt']
    15: name=['Energy(DW, ZF, GAM)','Energy','tot_ene_GAM.txt']
    18: name=['total '+'|$\Phi$| (GK)','|$\Phi$|','Phi_k.txt']
    29: name=['total D (CKinFH)','D','total_Dft.txt']
    30: name=['total '+'$\chi$$\downi$'+' (CKinFH)','$\chi$$\downi$','total_Chift.txt']
    31: name=['total Entropy (CKinFH)','Entropy','tot_Entropyft.txt']
    32: name=['total '+'|$\Phi$| (CKinFH)','|$\Phi$|','Phi_kft.txt']
    45: name=['total '+'|$\Phi$| (CKinCH)','|$\Phi$|','Phi_kcy.txt']
    46: name=['total entropy (CKinCH)','entropy','tot_Entropycy.txt']
    47: name=['Data processing','',' .txt']
    48: name=['Others by time','combine',' .txt']

    
    100: name=['density','density','den.out','real']
    101: name=['gamma_r','gamma_r','gamma_r.out','real']   
    102: name=['omega_mode','omega','omega_mode.out','complex']
    103: name=['density mode','density_mode','den_mode.out','complex']
    104: name=['phi mode','phi_mode','phi_mode.out','complex']
    
    106: name=['total '+'$\chi$$\downi$'+' (CKinCH)','$\chi$$\downi$','total_Chicy.txt']
    107: name=['total D (CKinCH)','D','total_Dcy.txt']
    109: name=['($\delta$n$\downe$/n$\down0$)$\up2$ (GK)','($\delta$n$\downe$/n$\down0$)$\up2$','sqrn_n0.txt']
    110: name=['($\delta$n$\downe$/n$\down0$)$\up2$ (CKinFH)','($\delta$n$\downe$/n$\down0$)$\up2$','sqrn_n0ft.txt']
    111: name=['($\delta$n$\downe$/n$\down0$)$\up2$ (CKinCH)','($\delta$n$\downe$/n$\down0$)$\up2$','sqrn_n0cy.txt']    
    112: name=['electrostatic Energy E (GK)','E$\upGK$','E.txt']
    113: name=['electrostatic Energy E (CKinFH)','E$\upFK$','Eft.txt']
    114: name=['electrostatic Energy E (CKinCH)','E$\upCK$','Ecy.txt']
            
    endcase
  Endif
  Endfor

  IF(satdelta_t EQ 0)then begin
  satdelta_t=sattime    ;;set the first left average line point to be the sattime
  Endif

  case (uvalue) of


     0:begin
     !Im_Re=0
     zoomz=zoombackup0
     goto, plot_it
     end

     1:begin
     !Im_Re=1
     zoomz=zoombackup1
     goto, plot_it
     end     
     
     
     2: begin
        !tplus=!tplus+1
        goto, plot_it
     end

     3: begin
        !tminus=!tminus+1
        goto, plot_it
     end
     
     4: begin
        !tplus=!tplus+10
        goto, plot_it
     end

     5: begin
        !tminus=!tminus+10
        goto, plot_it
     end      
     
     6: begin
        !tflag=!tflag+1
        IF(!tflag eq 2)then begin
        !tflag=0
        Endif
        goto, plot_it
     end
;
;
;     3: begin
;        !aveT = (!aveT+1) mod 2
;        goto, plot_it
;     end
;
;     4: begin
;        !aveP = (!aveP+1) mod 4
;        goto, plot_it
;     end

     7: begin
        zoomz = 2.0*zoomz
        goto, plot_it
     end

     8: begin
        zoomz = zoomz/2.0
        goto, plot_it
     end

     9: begin
        yrange_style=yrange_style+1
        goto, plot_it
     end

;     10: begin
;        i_units = 1-i_units
;        goto, plot_it
;     end

     10: begin
        log = (log+1) mod 2
        goto, plot_it
     end


     15: begin
     IF(!plotwindowsid eq 10)then begin
     Wdelete,!plotwindowsid
     !plotwindowsid=0
     Endif
     widget_control, event.top, /destroy
     end

     20: begin
        i_ptype = 0
        goto, plot_it
     end

     21: begin
        i_ptype = 1
        goto, plot_it
     end

  endcase

  return

  plot_it:

  ;;-------------------------------------------------------
  ;; PLOTTING
  ;;-------------------------------------------------------

IF(strcmp(name[3],'complex',7)) THEN BEGIN

  
  var_time_compl=complexarr(n_step_max/n_data+1 )
  var_temp=complex(0.0,0.0)
  n_temp=0
  openr,lun,name[2],/get_lun
  For i=0L,n_step_max/n_data Do begin
  readf,lun,n_temp,var_temp
  var_time_compl[i]=var_temp
  Endfor
  free_lun,lun
       

  If(!Im_Re eq 0)then begin
      name[0]='Re_'+name[0]
      !mtitle=name[0]

      y=fltarr(n_step_max/n_data+1)
      y(*)=REAL_PART(var_time_compl(*))      
      x=time
      
      xmax=max(x)
      xmin=min(x)
      ymax=max(REAL_PART(y(*)))
      ymin=min(REAL_PART(y(*)))

   print,'plot1line yrange_style=',yrange_style
   plot1line,x,y,'time (a/c'+'$\downs$'+')',name[1],0,n_step_max,del_t,n_data,satdelta_t,i_ptype,log,zoomz,yrange_style ;plot1line used to plot time trace of output data


   Endif Else IF(!Im_Re eq 1) THEN BEGIN
      name[0]='Im_'+name[0]
      !mtitle=name[0]
      
      y=fltarr(n_step_max/n_data+1)
      y(*)=imaginary(var_time_compl(*))      
      x=time
      
      !mtitle=name[0]      
       xmax=max(x)
       xmin=min(x)
       ymax=max(imaginary(y(*)))
       
;       If(min(imaginary(Omega_matr(*,*))) ge -0.5*max(imaginary(Omega_matr(*,*))))Then begin
       ymin=min(imaginary(y(*,*)))
;       Endif Else begin
;       ymin=-0.5*max(imaginary(Omega_matr(*,*)))
;       Endelse
;       ymax=ymax+(ymax-ymin)/5.0

   print,'plot1line yrange_style=',yrange_style
   plot1line,x,y,'time (a/c'+'$\downs$'+')',name[1],0,n_step_max,del_t,n_data,satdelta_t,i_ptype,log,zoomz,yrange_style ;plot1line used to plot time trace of output data
       
    Endif
    


Endif Else Begin


      y=dblarr(n_step_max/n_data+1)
      openr,lun,name[2],/get_lun
      readf,lun,y
      free_lun,lun
     ; y(0)=y(1)     ;ignore the first point of nt=0, because it's really a bad point, so I set the y(0)=y(1)
      x=time
      !mtitle=name[0]
   print,'plot1line yrange_style=',yrange_style
   plot1line,x,y,'time (a/c'+'$\downs$'+')',name[1],0,n_step_max,del_t,n_data,satdelta_t,i_ptype,log,zoomz,yrange_style ;plot1line used to plot time trace of output data



Endelse

cd,!vuM6DFolder  ;go to the vuM6D directory

 return
end

;*******************************************************************************


pro kspectrum1D_see,nametemp,windownum,group=group

;  common GLOBAL

  ;;-----------------------------------------------
  ;; Private (local) data
  ;;
  common plotaxis,zoomz,zoombackup0,zoombackup1,log,half,freq,i_ptype,satdelta_t,name,yrange_style
  common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark

  ;;-----------------------------------------------

  ;;------------------------------------------
  ;; Return conditions
  ;;
 ; if exists_diff eq 0 then return
 ; if xregistered('diffusion_ave_see') then return
  ;;------------------------------------------
  name=nametemp
  base = widget_base(title=name[0],$
                     /column)
   ;;set default value
  i_ptype=0
  log=0
  half=0
  freq=0
  zoomz = 1.0  ;zoomz is the parameter which used to adjust the range of the axis
  zoombackup0=1.0  ;zoombackup0 and zoombackup1 are used to set zoomz=1, when changes the plot between plot(re) and plot(im).
  zoombackup1=1.0
  satdelta_t=0.0  ;satdelta_t is the parameter which used to adjust the saturation time when the user click the button "t+" or "t-"
  yrange_style=1  ;yrange_style is the parameter which used to choose plot the top, middle or bottom part of the picture.
                  ;for the value of yrange_style/3, 0 for top, 1 for middle, 2 for bottom;
  defsysv,'!Im_Re',-1  ;;!Im_Re--to mark the Im or Re plot of complex number plot
  defsysv,'!Im_Re_backup',-1 ;;!Im_Re_backup --comparing with !Im_Re so as to decide whether a new sattime should be input.
  defsysv,'!Im_Re_count',0  ;; !Im_Re_count--comparing with !Im_Re_countBP so as to decide whether a new sattime should be input.
  defsysv,'!Im_Re_countBP',0  
  defsysv,'!sattime',1.0
  defsysv,'!MS_Init',0
  defsysv,'!threeD',0
  defsysv,'!tminus',0
  defsysv,'!tplus',0
  
  ;i_tp = 0
  ;;----------------------------------------------------------
  ;; BUTTONS
  ;;----------------------------------------------------------

  row1 = widget_base(base,$
                     /row,$
                     /frame)

 IF(strcmp(name[0],'den_mode',8) ) THEN BEGIN
  x = widget_button(row1, $
                    value='Plot(re)', $
                    uvalue=0)

  x = widget_button(row1, $
                    value='Plot(im)', $
                    uvalue=1)
 Endif Else Begin
  x = widget_button(row1, $
                    value='Plot', $
                    uvalue=0)
 Endelse


  x = widget_button(row1, $
                    value='ZOOM in', $
                    uvalue=3)

  x = widget_button(row1, $
                    value='ZOOM out', $
                    uvalue=4)
  x = widget_button(row1, $
                    value='Yrange', $
                    uvalue=5)
 
  x = widget_button(row1, $
                    value='t+', $
                    uvalue=6)

  x = widget_button(row1, $
                    value='t-', $
                    uvalue=7)
                    
   x = widget_button(row1, $
                    value='t+ +', $
                    uvalue=8)

  x = widget_button(row1, $
                    value='t- -', $
                    uvalue=9)
                    
                                        
  x = widget_button(row1,$
                    value='log',$
                    uvalue=10)


;  x = widget_button(row1, $
;                    value='3D', $
;                    uvalue=9)

  x = widget_button(row1, $
                    value='Done', $
                    uvalue=15)
  ;;----------------------------------------------------------
  ;; DRAW WIDGET and CONTROL
  ;;----------------------------------------------------------

  draw = widget_draw(base,     $
                     xsize=750, $
                     ysize=650)

  widget_control, base, $
    ;set_uvalue=state,$
    /no_copy, $
    /realize

  ;!plotkspecid=!D.WINDOW
    (*windowmark)[windownum]=!D.window
    basemark[windownum]=base

  xmanager,'kspectrum1D', $
    base,$
;    event='energy_time_trace_event',$
    group_leader=group


end


;*******************************************************************************
pro kspectrum1D_event,event


  common startup,number_plot,fpath,ncolor,color_value,plotid
  common plotaxis,zoomz,zoombackup0,zoombackup1,log,half,freq,i_ptype,satdelta_t,name,yrange_style
  common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark

  widget_control, event.id, $
    get_uvalue=uvalue
 ; wset, widget

cd,!workFolder
openr,lun,'M6D.input',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_parameter',plot_name_temp,19)
  endwhile
  thisFormat='(8x,i2,/,7x,i9,/,8x,i2,/,9x,i2)'
  readf,lun,restart,n_data,IC_flag,i_solver,Format=thisFormat
  
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Grid_parameter',plot_name_temp,16)
  endwhile
  thisFormat='(11x,i15,/,6x,f12.8,/,4x,i5,/,4x,i5,/,8x,i5,/,4x,i5,/,6x,i5,/,6x,i5,/,5x,i5,/,5x,i5)'
  readf,lun,n_step_max,del_t,n_g,n_r,n_theta,n_n,n_min,n_del,n_v1,n_v3,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Physics_parameter',plot_name_temp,19)
  endwhile
  thisFormat='(11x,f7.4,/,8x,f7.4,/,8x,f7.4,/,10x,f7.4,/,6x,f5.3,/,6x,f8.3,/,5x,f12.7,/,5x,f12.7,/,7x,f12.7,/,6x,f12.7,/,6x,f8.4,/,8x,f12.7)'
  readf,lun,Omega_star,r_hat_a,r_hat_b,Rmaj0_hat,q_mid,s_hat,aoLn,aoLT,nu_mid,v_max,delta,delta_i, Format=thisFormat

free_lun,lun


cd,!vuM6DFolder
openr,lun,'vuM6D.input',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_variables',plot_name_temp,19)
  endwhile
  thisFormat='(7x,i5,/,9x,i5,/,8x,f10.3,/,14x,i2,/,13x,i2,/,/,/,4x,i6,/,4x,i6,/,11x,i4,/,13x,i2,/,5x,i5,/,8x,i2,/,2x,i4,/,2x,i4)'
  readf,lun,avenum,plotstep,sattime,Intermit_Plot,overplot_log,ix1,ix2,Switch_sat,quicktrigger,jump,control,k_theta,p_r,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Range_control',plot_name_temp,15)
  endwhile
  thisFormat='(10x,f15.12,/,10x,f12.4,/,11x,f15.10,/,11x,f15.12,/,9x,i5,/,8x,f10.4,/,10x,f12.3)'
  readf,lun,Omega_min,Omega_max,Phi_logmin,Chi_logmin,OMGpoint,freqPer,begintime,Format=thisFormat

free_lun,lun

cd,!workFolder

   timetemp1=0.0
   timetemp2=0.0
   openr,lun,'time.out',/get_lun 
   readf,lun,timetemp1
   readf,lun,timetemp1
   POINT_LUN, -lun, pos1
   readf,lun,timetemp2
   POINT_LUN, -lun, pos2
   free_lun,lun  
   
   openr,lun,'time.out',/get_lun,/APPEND 
   POINT_LUN, -lun, posEnd
   POINT_LUN, lun, posEnd-(pos2-pos1)
   readf,lun,Endtime
   print,'Endtime=',Endtime
   free_lun,lun

   n_step_max=round(Endtime/del_t)
   
   Time=fltarr(n_step_max/n_data+1)
   openr,lun,'time.out',/get_lun
   readf,lun,time
   free_lun,lun  

  
   theta=findgen(n_theta)/n_theta*2 - 1. 
   r=findgen(n_r)/n_r*(r_hat_b-r_hat_a)+r_hat_a


  For i=0,200-1 Do begin
  IF(basemark[i] eq event.top)then begin
    IF((*windowmark)[i] ne !D.window)then begin
    wset,(*windowmark)[i]
    Endif
    case(i) of
    105: name=['den_mode_theta','den_mode','den_mode3D.out']
    106: name=['den_mode_r','den_mode','den_mode3D.out']
   
    endcase
  Endif
  Endfor

  case (uvalue) of

;  IF(strcmp(name[0],'Omega_',6)) THEN BEGIN
     0:begin
     !Im_Re=0
     zoomz=zoombackup0
     !Im_Re_count=!Im_Re_count+1     
     goto, plot_it
     end

     1:begin
     !Im_Re=1
     zoomz=zoombackup1
     !Im_Re_count=!Im_Re_count+1     
     goto, plot_it
     end

     2: begin
        half=(half+1) mod 2
        goto, plot_it
     end

     3: begin
        zoomz = 2.0*zoomz
        IF(!Im_Re eq 0) THEN BEGIN
        zoombackup0=zoomz
        Endif Else If(!Im_Re eq 1)THEN Begin
        zoombackup1=zoomz
        Endif
        goto, plot_it
     end

     4: begin
        zoomz = zoomz/2.0
        IF(!Im_Re eq 0) THEN BEGIN
        zoombackup0=zoomz
        Endif Else If(!Im_Re eq 1)THEN Begin
        zoombackup1=zoomz
        Endif
        goto, plot_it
     end

     5: begin
        yrange_style=yrange_style+1
        goto, plot_it
     end


     6: begin
        !tplus=!tplus+1
        goto, plot_it
     end

     7: begin
        !tminus=!tminus+1
        goto, plot_it
     end
     
     8: begin
        !tplus=!tplus+10
        goto, plot_it
     end

     9: begin
        !tminus=!tminus+10
        goto, plot_it
     end     


     10: begin
        log = (log+1) mod 3
        goto, plot_it
     end




;     9: begin
;        !threeD = (!threeD + 1) mod 2
;        goto, plot_it
;     end
     
     15: begin ;shut down this window
;     IF(!plotwindowsid eq 10)then begin
;     Wdelete,!plotwindowsid
;     !plotwindowsid=0
;     Endif
     widget_control, event.top, /destroy
     end

  endcase

  return

  plot_it:

  ;;-------------------------------------------------------
  ;; PLOTTING
  ;;-------------------------------------------------------
  
  tempReCon=8
  
;  ntnum_of_bin=round(ntmax/(output_step*avenum))
  !p.background=color_value(ncolor/2)
;=================================================================================================================
  IF((!Im_Re_backup ne !Im_Re) or (!Im_Re_count ne !Im_Re_countBP))then begin
  Print,''
  Print,'Input the sattime:'
  read,sattimetemp
  !sattime=sattimetemp
  Endif

  If(!sattime GE 0)then Begin
  sattime=!sattime
  Endif

  den_mode3D=Dcomplexarr(n_theta,n_r,n_n,n_step_max/n_data+1)
  openr,lun,'den_mode3D.out',/get_lun
  readf,lun,den_mode3D
  free_lun,lun
  
  IF(strcmp(name[0],'den_mode_theta',14)) THEN BEGIN
  p_k_flag=p_r
  print, 'p=',round(p_k_flag) 
  Endif
  
  IF(strcmp(name[0],'den_mode_r',10)) THEN BEGIN
  p_k_flag=k_theta
  print, 'k=', round(p_k_flag)
  Endif

  
  jstart=sattime/del_t/n_data +!tplus -!tminus
  
  If(jstart ge Endtime/del_t/n_data) Then begin
  jstart=jstart-Endtime/del_t/n_data
  Endif else if(jstart lt 0) Then begin
  jstart=jstart+Endtime/del_t/n_data 
  Endif
  
  
  
  If(!Im_Re eq 0)then begin
    name[0]='Re_'+name[0]
    !mtitle=name[0]
    IF(strcmp(name[0],'Re_den_mode_theta',17)) THEN BEGIN
    x=theta
    xtitle='$\theta$ (x'+'$\pi$'+')'        
    Endif else if (strcmp(name[0],'Re_den_mode_r',13)) Then Begin
    x=r
    xtitle='r/a '  
    Endif

    xmax=max(x)
    xmin=min(x)
     ;ymax=max(REAL_PART(den_mode3D(*,0,0,jstart:jstart+(avenum-1)*jump)))
     ;ymin=min(REAL_PART(den_mode3D(*,0,0,jstart:jstart+(avenum-1)*jump)))
    ymax=1
    ymin=-1
    
    For i=0,avenum-1 Do Begin
      j=jstart+i*jump
        IF(strcmp(name[0],'Re_den_mode_theta',17)) THEN BEGIN
        y=REAL_PART(den_mode3D(*,p_k_flag,0,j))
        y(*)=y(*)/total(abs(den_mode3D(*,p_k_flag,0,j)))*n_theta
        Endif else if (strcmp(name[0],'Re_den_mode_r',13)) Then Begin
        y=REAL_PART(den_mode3D(p_k_flag,*,0,j))
        y(*)=y(*)/total(abs(den_mode3D(p_k_flag,*,0,j)))*n_r           
        Endif
      overplot,x,y,xtitle,'density',xmax,xmin,ymax,ymin,0,log,i,jstart,j,zoomz,yrange_style,0
    Endfor

  Endif Else IF(!Im_Re eq 1) THEN BEGIN
    name[0]='Im_'+name[0]
    !mtitle=name[0]  
    IF(strcmp(name[0],'Im_den_mode_theta',17)) THEN BEGIN
    x=theta
    xtitle='$\theta$ (x'+'$\pi$'+')'        
    Endif else if (strcmp(name[0],'Im_den_mode_r',13)) Then Begin
    x=r
    xtitle='r/a '  
    Endif  
  
    xmax=max(x)
    xmin=min(x)
    ; ymax=max(imaginary(den_mode3D(*,0,0,jstart:jstart+(avenum-1)*jump)))
    ; ymin=min(imaginary(den_mode3D(*,0,0,jstart:jstart+(avenum-1)*jump)))
    ymax=1
    ymin=-1      
    For i=0,avenum-1 Do Begin
      j=jstart+i*jump
        IF(strcmp(name[0],'Im_den_mode_theta',17)) THEN BEGIN
        y=imaginary(den_mode3D(*,p_k_flag,0,j))
        y(*)=y(*)/total(abs(den_mode3D(*,p_k_flag,0,j)))*n_theta
        Endif else if (strcmp(name[0],'Im_den_mode_r',13)) Then Begin
        y=imaginary(den_mode3D(p_k_flag,*,0,j))
        y(*)=y(*)/total(abs(den_mode3D(p_k_flag,*,0,j)))*n_r       
        Endif
      overplot,x,y,xtitle,'density',xmax,xmin,ymax,ymin,0,log,i,jstart,j,zoomz,yrange_style,0
    Endfor   
      
   Endif

      lines = indgen(avenum)                    ; for line styles
      line = findgen(avenum)*jump*n_data*del_t+ jstart*(del_t*n_data)
      items = 'time='+strtrim(line,2)           ; annotations
      legend,items,linestyle=lines,charsize=1.5;,/bottom      ; at lower left



; IF(!threeD eq 1)THEN BEGIN
;  IF(!Im_Re eq 0) THEN BEGIN
;   !p.background=color_value(ncolor/2)
;   window, plotidh+2, TITLE='main_plot', xsize=500,ysize=500
;      x=kxhalf
;      y=kyhalf
;      z=abs(REAL_PART(Omega_matr(*,*)))
;      !mtitle=name[0]
;      surface3D,z,x,y,'kx','ky',0,1,0,Az,Ax
;  Endif
;  IF(!Im_Re eq 1) THEN BEGIN
;   !p.background=color_value(ncolor/2)
;   window, plotidh+2, TITLE='main_plot', xsize=500,ysize=500
;      x=kxhalf
;      y=kyhalf
;      z=gammad(*,*)
;      !mtitle=name[0]
;      surface3D,z,x,y,'kx','ky',0,1,0,Az,Ax
;  Endif
; ENDIF 

;=================================================================================================================
  !Im_Re_countBP=!Im_Re_count
  !Im_Re_backup=!Im_Re
  cd,!vuM6DFolder
  
  return
end

;*******************************************************************************
pro Main_Plots_event,event

cd, !workFolder

openr,lun,'M6D.input',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_parameter',plot_name_temp,19)
  endwhile
  thisFormat='(8x,i2,/,7x,i9,/,8x,i2,/,9x,i2)'
  readf,lun,restart,n_data,IC_flag,i_solver,Format=thisFormat
  

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Grid_parameter',plot_name_temp,16)
  endwhile
  thisFormat='(11x,i15,/,6x,f12.8,/,4x,i5,/,4x,i5,/,8x,i5,/,4x,i5,/,6x,i5,/,6x,i5,/,5x,i5,/,5x,i5)'
  readf,lun,n_step_max,del_t,n_g,n_r,n_theta,n_n,n_min,n_del,n_v1,n_v3,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Physics_parameter',plot_name_temp,19)
  endwhile
  thisFormat='(11x,f7.4,/,8x,f7.4,/,8x,f7.4,/,10x,f7.4,/,6x,f5.3,/,6x,f8.3,/,5x,f12.7,/,5x,f12.7,/,7x,f12.7,/,6x,f12.7,/,6x,f8.4,/,8x,f12.7)'
  readf,lun,Omega_star,r_hat_a,r_hat_b,Rmaj0_hat,q_mid,s_hat,aoLn,aoLT,nu_mid,v_max,delta,delta_i, Format=thisFormat

free_lun,lun


cd, !vuM6DFolder 
openr,lun,'vuM6D.input',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_variables',plot_name_temp,19)
  endwhile
  thisFormat='(7x,i5,/,9x,i5,/,8x,f10.3,/,14x,i2,/,13x,i2,/,/,/,4x,i6,/,4x,i6,/,11x,i4,/,13x,i2,/,5x,i5,/,8x,i2)'
  readf,lun,avenum,plotstep,sattime,Intermit_Plot,overplot_log,ix1,ix2,Switch_sat,quicktrigger,jump,control,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Range_control',plot_name_temp,15)
  endwhile
  thisFormat='(10x,f15.12,/,10x,f12.4,/,11x,f15.10,/,11x,f15.12,/,9x,i5,/,8x,f10.4,/,10x,f12.3)'
  readf,lun,Omega_min,Omega_max,Phi_logmin,Chi_logmin,OMGpoint,freqPer,begintime,Format=thisFormat

free_lun,lun

cd,!workFolder

   timetemp1=0.0
   timetemp2=0.0
   openr,lun,'time.out',/get_lun 
   readf,lun,timetemp1
   readf,lun,timetemp1
   POINT_LUN, -lun, pos1
   readf,lun,timetemp2
   POINT_LUN, -lun, pos2
   free_lun,lun  
   
   openr,lun,'time.out',/get_lun,/APPEND 
   POINT_LUN, -lun, posEnd
   POINT_LUN, lun, posEnd-(pos2-pos1)
   readf,lun,Endtime
   print,'Endtime=',Endtime
   free_lun,lun
   
   
   n_step_max=round(Endtime/del_t)
   
   Time=fltarr(n_step_max/n_data+1)
   openr,lun,'time.out',/get_lun
   readf,lun,time
   free_lun,lun  
   

  common startup,number_plot,fpath,ncolor,color_value,plotid
  common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark

  widget_control,event.id,get_uvalue=choice
    !p.thick=2

  case choice of

    0: begin 
    nametemp=['density','density','den.out','real']
    twoD_time_trace_see,nametemp,100
    end

    1: begin 
    nametemp=['gamma_r','gamma_r','gamma_r.out','real']
    twoD_time_trace_see,nametemp,101
    end
    
    2: begin 
    nametemp=['omega_mode','omega','omega_mode.out','complex']
    twoD_time_trace_see,nametemp,102
    end    
    
    3: begin 
    nametemp=['density mode','density_mode','den_mode.out','complex']
    twoD_time_trace_see,nametemp,103
    end  
    

    4: begin 
    nametemp=['phi mode','phi_mode','phi_mode.out','complex']
    twoD_time_trace_see,nametemp,104
    end  
    
    
    5: begin
    nametemp=['den_mode_theta','den_mode','den_mode3D.out']
    kspectrum1D_see,nametemp,105
    end

    6: begin
    nametemp=['den_mode_r','den_mode','den_mode3D.out']
    kspectrum1D_see,nametemp,106
    end


           
    10: begin ;total_Chi (CKinCH)
    nametemp=['total Chi (CKinCH)','Chi','total_Chicy.txt']
    twoD_time_trace_see,nametemp,106
    end
    
    11: begin ;total_D (CKinCH)
    nametemp=['total D (CKinCH)','D','total_Dcy.txt']
    twoD_time_trace_see,nametemp,107
    end

    14: begin ;Omega (CKinCH)
    nametemp=['Omegacy(ky)','ky','Phi_kcy.txt']
    ;;nametemp include: title, ytitle, and input data file name
     kspectrum1D_see,nametemp,108
    end
    
        
    
    7: begin
    nametemp=['(ne/n0)^2 (CKinFH)','(ne/n0)^2','absn_n0ft.txt']
    twoD_time_trace_see,nametemp,110
    end 
    
    8: begin 
    nametemp=['electrostatic Energy E (CKinFH)','E$\upFK$','Eft.txt']
    twoD_time_trace_see,nametemp,113
    end            
                
    9: begin ;Omega (CKinFH)
    nametemp=['Omegaft(ky)','ky','Phi_kcy.txt']
    ;;nametemp include: title, ytitle, and input data file name
     kspectrum1D_see,nametemp,109
    end   


    12: begin 
    nametemp=['(ne/n0)^2 (CKinCH)','(ne/n0)^2','absn_n0cy.txt']
    twoD_time_trace_see,nametemp,111
    end 

    3: begin 
    nametemp=['electrostatic Energy E (GK)','E$\upGK$','E.txt']
    twoD_time_trace_see,nametemp,112
    end 
        
        
    13: begin 
    nametemp=['electrostatic Energy E (CKinCH)','E$\upCK$','Ecy.txt']
    twoD_time_trace_see,nametemp,114
    end 
                            

    

            

    
  endcase
end

;*******************************************************************************
pro eplot,x,y,sigyup,sigylo,_extra=_extra,barlinestyle=barlinestyle, $
          color=color,linestyle=linestyle,thick=thick,noclip=noclip, $
          t3d=t3d
;+
; NAME:
;
;       EPLOT
;
; PURPOSE:
;
;       Plot x vs y, with vertical error bars on y.
;
; CALLING SEQUENCE:
;
;       EPLOT,Y,SIGY
;       EPLOT,X,Y,SIGY
;       EPLOT,Y,SIGY_UP,SIGY_DOWN
;       EPLOT,X,Y,SIGY_UP,SIGY_DOWN
;
; INPUTS:
;
;       X, Y -  1-D arrays
;
;       SIGY - Uncertainty in Y, i.e. Y+/-SIGY
;
;       SIGY_UP, SIGY_DOWN - +/- uncertainties in Y, i.e.,
;                           Y +SIGY_UP -SIGY_DOWN
;
; KEYWORD PARAMETERS:
;
;       BARLINESTYLE = Linestyle for error bars.
;
;               plus all valid IDL plot keywords.  Only the COLOR,
;               THICK, NOCLIP, and T3D keywords apply to the error
;               bars.
;
; MODIFICATION HISTORY:
;
;      D. L. Windt, Bell Laboratories, November 1989
;      Replaced specific plot/oplot keywords with _EXTRA,
;      April, 1997
;
;      windt@bell-labs.com
;-
on_error,2

if n_params() lt 3 then message,'Usage: EPLOT,X,Y,SIGY'

if n_elements(color) eq 0 then color=!p.color
if n_elements(linestyle) eq 0 then linestyle=!p.linestyle
if n_elements(thick) eq 0 then thick=!p.thick
if n_elements(noclip) eq 0 then noclip=!p.noclip
if n_elements(t3d) eq 0 then t3d=!p.t3d
if n_elements(barlinestyle) eq 0 then barlinestyle=linestyle

plot,x,y,_extra=_extra, $
  color=color,linestyle=linestyle,thick=thick,noclip=noclip,t3d=t3d
psym=4 ;!p.psym
!p.psym=0
xt=fltarr(2)
xb=fltarr(2)
yt=xt
if n_params() eq 3 then sigylo=sigyup
for i=0,n_elements(x)-1 do begin
    xt(0)=x(i)
    xt(1)=x(i)
    yt(0)=y(i)
    yt(1)=y(i)+sigyup(i)
    oplot,xt,yt, $
      color=color,linestyle=barlinestyle,thick=thick,noclip=noclip,t3d=t3d
;    xb(0)=x(i);-(x(n_elements(x)-1)-x(0))/60
;    xb(1)=x(i)+(x(n_elements(x)-1)-x(0))/60
;    oplot,xb,yt(1), $
;      color=color,linestyle=barlinestyle,thick=thick,noclip=noclip,t3d=t3d
    yt(1)=y(i)-sigylo(i)
    oplot,xt,yt, $
      color=color,linestyle=barlinestyle,thick=thick,noclip=noclip,t3d=t3d
;    oplot,xb,yt(1), $
;      color=color,linestyle=barlinestyle,thick=thick,noclip=noclip,t3d=t3d
endfor
;   plot,x(3),y(3),psym=4,symsize=2.5
!p.psym=psym
return
end
;*******************************************************************************
;*******************************************************************************
;
;pro surface3D1,z,x,y,x_title,y_title,nbegin,logtezoomzemp
;
;  common startup,number_plot,fpath,ncolor,color_value,plotid
;
;  zmax=max(z)
;  zmin=min(z)
;  xmax=max(x)
;  xmin=min(x)
;  ymax=max(y)
;  ymin=min(y)
;;  if !D.name eq 'X' then wset,plotid
;  !noeras=0
;  !p.color=color_value(ncolor+1)
;;  set_viewport,0.15,0.95,0.15,0.9
;;  set_xy,xmin,xmax,ymin,ymax
;;
;  if logtemp eq 1 then surface,z,x,y,$
;       Skirt=0,xtitle=x_title,$
;       xrange=[xmin,xmax],$
;       yrange=[ymin,ymax],$
;       zrange=[zmin,zmax]/zoomtemp,$
;       ytitle=y_title,charsize=2.0,/zlog
;
;  if logtemp eq 0 then surface,z,x,y,$
;       Skirt=0,xtitle=x_title,$
;       xrange=[xmin,xmax],$
;       yrange=[ymin,ymax],$
;       zrange=[zmin,zmax]/zoomtemp,$
;       ytitle=y_title,charsize=2.0
;
;  return
;end
;********************************************************************************

pro surface3D,z,x,y,x_title,y_title,nbegin,ntime,logtemp,Aztemp,Axtemp,zminlog

  common startup,number_plot,fpath,ncolor,color_value,plotid

  zmax=max(z)
  zmin=min(z)
  xmax=max(x)
  xmin=min(x)
  ymax=max(y)
  ymin=min(y)
;
;  if !D.name eq 'X' then wset,plotid
  !noeras=0
  !p.color=color_value(ncolor+1)
;  set_viewport,0.15,0.95,0.15,0.9
;  set_xy,xmin,xmax,ymin,ymax
;
  if logtemp eq 1 then surface,z,x,y,Skirt=0,$
     xrange=[xmin,xmax],$
     yrange=[ymin,ymax],$
     zrange=[zminlog,zmax],$
     Az=Aztemp,Ax=Axtemp,$
     xtitle=x_title,ytitle=y_title,charsize=3.0,thick=1.0,/zlog
  if logtemp eq 0 then surface,z,x,y,Skirt=0,$
     xrange=[xmin,xmax],yrange=[ymin,ymax],$
     zrange=[zmin,zmax],$
     Az=Aztemp,Ax=Axtemp,$
     xtitle=x_title,ytitle=y_title,charsize=3.0,thick=1.0

  return
end

;*******************************************************************************

pro plot1line,x,y,x_title,ytitle,nbegin,n_step_max,del_t,n_data,sattime,i_ptype,logtemp,zoomtemp,yrange_style

  common startup,number_plot,fpath,ncolor,color_value,plotid
  common plot_variables,y_bin,pdf,nbin,d_y


  xmax=max(x)
  xmin=min(x)
  ymin=min(y)
  
  IF(!point1 eq 0)then begin
  !point1=sattime  ;initialize ix1 and ix2
  Endif
  IF(!point2 eq 0)then begin
  !point2=max(x)
  Endif
  
  IF(!tflag eq 0)then begin
  !point1=sattime + !tplus*(n_data*2)*del_t - !tminus*(n_data*1)*del_t
  
    if (!point1 ge (n_step_max)*del_t or !point1 le 0) then begin
    !tplus=0
    !tminus=0
    !point1=sattime
    endif
  Endif  

  IF(!tflag eq 1)then begin
  !point2=max(x)+ !tplus*(n_data*1)*del_t - !tminus*(n_data*2)*del_t
    if (!point2 gt max(x)) then begin
    !tplus=0
    !tminus=0
    !point2 =max(x)
    endif
  Endif
 
  ix1=!point1
  ix2=!point2
   
  
  xxx1=round(ix1/(n_data*del_t))
  xxx2=round(ix2/(n_data*del_t))
  ymax=max(y(xxx1:xxx2))*2


; !point1=sattime
; !point4=max(x)
; !point2=(sattime+max(x))/2.0
; !point3=(sattime+max(x))/2.0


 ; plotid=number_plot
 ; print,'plotid========',plotid
 ;if !D.name eq 'X' then wset,plotid
  !noeras=0
  !p.color=color_value(ncolor+1)
  set_viewport,0.15,0.95,0.2,0.9
  set_xy,xmin,xmax,ymin,ymax
  !p.color=color_value(ncolor+1)

   if i_ptype eq 0 then begin
     ;; Diffusion TIME trace
   yrangetemp=Dblarr(2)
   if yrange_style eq 1 then begin  ;;plot from the min one of the data
   yrangetemp=[1.2*ymin/zoomtemp,1.2*ymax/zoomtemp]
   endif   
   if yrange_style eq 2 then begin  ;;plot upto the max one of the data
   yrangetemp=[-1.1*(ymax-ymin)/zoomtemp, 1.1*(ymax-ymin)/zoomtemp]
   endif
   if yrange_style eq 3  then begin  ;;plot the middle part of the data
   yrangetemp=[0.00001, 1.1*ymax/zoomtemp]
   endif


   if yrange_style ge 3  then begin
   yrange_style=yrange_style-3
   endif
   

  if logtemp eq 1 then  cgplot,[0],[0],$
       /nodata,$
       xstyle=1,$
      ; xminor=0,$
       xrange=[xmin,xmax],$
;       xrange=[min(x),max(x)],$
       xtitle=x_title,$
       ystyle=1,$
       yminor=0,$
     yrange=yrangetemp,$
    ;  yrange=[0.01,10000],$
     title=!mtitle,$
       ytitle=ytitle, $
       /ylog
;       color=line

  if logtemp eq 0 then  cgplot,[0],[0],$
       /nodata,$
       xstyle=1,$
       ;xminor=0,$
       xrange=[min(x),max(x)],$
       xtitle=x_title,$
       ystyle=1,$
       yminor=0,$
       yrange=yrangetemp,$
       title=!mtitle,$
       ytitle=ytitle

    ;; Diffusion trace
;     !p.color=color_value(ncolor+1)
     cgplot,x,y,thick=2,/overplot;,color=color_vec[0]


     ;; Average line
      ;ix2=max(x)
      xxx1=round(ix1/(n_data*del_t))
      xxx2=round(ix2/(n_data*del_t))
      xxx1_2=xxx2-xxx1+1
      sat_ave=total(y(xxx1:xxx2))/xxx1_2
      ysat_ave=fltarr(2)
      ysat_ave[*]=sat_ave
;
;delta_Sat=sqrt(total((y(xxx1:xxx2)-sat_ave)^2)/xxx1_2 )
;Intermittency=delta_Sat/sat_ave
;print, ' from',ix1,'s to',ix2,'s       sat_ave & delta_Sat=',sat_ave,delta_Sat
;print, ' from',ix1,'s to',ix2,'s Intermittency=', Intermittency
;print,' '

   !p.color=color_value(ncolor*11/16) ;red
     cgplot,[ix1,ix2],ysat_ave,thick=2.5,Color='red',/overplot
   xyouts,ix1*0.8,ysat_ave+(yrangetemp[1]-yrangetemp[0])/60,ysat_ave,charthick=3.8,size=2.6  ;+'+/-'+strtrim(delta_Sat,2)



;  !p.color=color_value(ncolor+1)
;   xyouts,xmin,yrangetemp[1]-(yrangetemp[1]-yrangetemp[0])/15,'Intermittency='+strtrim(Intermittency,2)
;     ;; RMS deviation bars
;
;      ixmid=ix1+(ix2-ix1)/2.0
;      xxxmid=round(ixmid/(output_step*tstep))
;      xxx1_mid=xxxmid-xxx1+1
;      xxxmid_2=xxx2-xxxmid+1
;      sat_ave_front=total(y(xxx1:xxxmid))/xxx1_mid
;      sat_ave_back=total(y(xxxmid:xxx2))/xxxmid_2
;      ysat_ave_front=fltarr(2)
;      ysat_ave_front[*]=sat_ave_front
;      ysat_ave_back=fltarr(2)
;      ysat_ave_back[*]=sat_ave_back
;     !p.color=color_value(ncolor*1/8) ;green
;     cgplot,[ix1,ixmid],ysat_ave_front,thick=2.0,Color='green',/overplot
;   xyouts,ix1,ysat_ave_front-(yrangetemp[1]-yrangetemp[0])/20,strtrim(ysat_ave_front,2),charthick=3.8,size=2.6
;      !p.color=color_value(ncolor*1/8)
;     cgplot,[ixmid,ix2],ysat_ave_back ,thick=2.0,Color='green',/overplot
;   xyouts,ixmid,ysat_ave_back+(yrangetemp[1]-yrangetemp[0])/70,ysat_ave_back,charthick=3.8,size=2.6
;     !p.color=color_value(ncolor+1)  ;black



      endif else begin
  ;; Diffusion PDF histogram

      nbin=15  ;set default value
      it1=max(x)/2.0+ix1
      it2=max(x)

     pdf_statistics,x,y,it1,it2,tstep,output_step;,y_bin,pdf

     y_bin_plot = fltarr(2*nbin)
     pdf_plot   = fltarr(2*nbin)

     i  = indgen(nbin)
     dy = y_bin[1]-y_bin[0]

     y_bin_plot[2*i]   = y_bin[i]-0.5*dy
     y_bin_plot[2*i+1] = y_bin[i]+0.5*dy
     pdf_plot[2*i]     = pdf[i]
     pdf_plot[2*i+1]   = pdf[i]

     xmin = min(y_bin_plot)
     xmax = max(y_bin_plot)
     ymin = min(pdf_plot)
     ymax = max(pdf_plot)

     yave = total(y_bin[*]*pdf[*])
  if logtemp eq 1 then  plot,[0],[0],$
       /nodata,$
;       title=title,$
       xstyle=1,$
       xminor=0,$
       xrange=[xmin,xmax],$
       xtitle=ytitle,$
       ystyle=1,$
       yminor=0,$
       yrange=[ymin,ymax],$
       ytitle='!3Probability Density Function (saturated state)', $
       color=line,$
       /ylog

   if logtemp eq 0 then  plot,[0],[0],$
       /nodata,$
;       title=title,$
       xstyle=1,$
       xminor=0,$
       xrange=[xmin,xmax],$
       xtitle=ytitle,$
       ystyle=1,$
       yminor=0,$
       yrange=[ymin,ymax],$
       ytitle='!3Probability Density Function (saturated state)', $
       color=line


     oplot,y_bin_plot,pdf_plot;,color=color_vec[0]
       !p.color=color_value(ncolor*13/16)
     oplot,yave*[1,1],100*[-1,1],linestyle=1,thick=1.5
     xyouts,yave-(xmax-xmin)/4.0,(ymin+3*ymax)/4.0,'average value of saturate state',size=1.5
  endelse


;==================================================================================


IF(!aveT eq 1)then begin
  xmax=max(x)
  xmin=min(x)
  ymin=min(y)

 print,'!tplus=',!tplus
 print,'!tminus=',!tminus

 ;get the four points
 print,' '
 print,'Point',!aveP+1
 print,' '
 IF(!aveP eq 0)then begin
  !point1=sattime + !tplus*(ntmax/40)*tstep - !tminus*(ntmax/60)*tstep
  if (!point1 ge !point2 or !point1 le 0) then begin
  !tplus=0
  !tminus=0
  !point1=min(sattime,!point2)
  endif
 Endif
 IF(!aveP eq 1)then begin
  !point2=(sattime+max(x))/2.0 + !tplus*(ntmax/40)*tstep - !tminus*(ntmax/60)*tstep
  if (!point2 ge !point3 or !point2 le !point1) then begin
  !tplus=0
  !tminus=0
  !point2 =max(!point1,min((sattime+max(x))/2.0,!point3))
  endif
 Endif
 IF(!aveP eq 2)then begin
  !point3=(sattime+max(x))/2.0 + !tplus*(ntmax/40)*tstep - !tminus*(ntmax/60)*tstep
  if (!point3 ge !point4 or !point3 le !point2) then begin
  !tplus=0
  !tminus=0
  !point3 =max(!point2,min((sattime+max(x))/2.0,!point4))
  endif
 Endif
 IF(!aveP eq 3)then begin
  !point4=max(x) + !tplus*(ntmax/40)*tstep - !tminus*(ntmax/60)*tstep
  if (!point4 ge max(x) or !point4 le !point3) then begin
  !tplus=0
  !tminus=0
  !point4 =max(x)
  endif
 Endif


  point1=round(!point1/(output_step*tstep))
  point4=round(!point4/(output_step*tstep))
  ymax=max(y(point1:point4))*1.1


 ; plotid=number_plot
 ; print,'plotid========',plotid
 ;if !D.name eq 'X' then wset,plotid
  !noeras=0
  !p.color=color_value(ncolor+1)
  set_viewport,0.15,0.95,0.2,0.9
  set_xy,xmin,xmax,ymin,ymax
  !p.color=color_value(ncolor+1)


   if i_ptype eq 0 then begin
     ;; Diffusion TIME trace
   yrangetemp=Dblarr(2)
   if yrange_style ge 3  then begin
   yrange_style=yrange_style-3
   endif
   if yrange_style eq 1  then begin  ;;plot the middle part of the data
   yrangetemp=[0, 1.1*ymax/zoomtemp]
   endif
   if yrange_style eq 2 then begin  ;;plot from the min one of the data
   yrangetemp=[0.1*ymin/zoomtemp,1.1*ymax/zoomtemp]
   endif
   if yrange_style eq 0 then begin  ;;plot upto the max one of the data
   yrangetemp=[0, 1.1*ymax/zoomtemp*0.8]
   endif

  if logtemp eq 1 then  cgplot,[0],[0],$
       /nodata,$
       xstyle=1,$
       xminor=0,$
       xrange=[xmin,xmax],$
;       xrange=[min(x),max(x)],$
       xtitle=x_title,$
       ystyle=1,$
       yminor=0,$
     yrange=yrangetemp,$
      title=!mtitle,$
       ytitle=ytitle, $
       /ylog
;       color=line

  if logtemp eq 0 then  cgplot,[0],[0],$
       /nodata,$
       xstyle=1,$
       xminor=0,$
       xrange=[min(x),max(x)],$
       xtitle=x_title,$
       ystyle=1,$
       yminor=0,$
     yrange=yrangetemp,$
      title=!mtitle,$
      ytitle=ytitle

    ;; Diffusion trace
;     !p.color=color_value(ncolor+1)
     cgplot,x,y,thick=1.2,/overplot;,color=color_vec[0]



      point1=round(!point1/(output_step*tstep))
      point2=round(!point2/(output_step*tstep))
      num1_2=point2-point1+1
      ave1_2=total(y(point1:point2))/num1_2
      yave1_2=fltarr(2)
      yave1_2[*]=ave1_2
   !p.color=color_value(ncolor*11/16) ;red
   cgplot,[!point1,!point2],yave1_2,thick=1.5,Color='red',/overplot
   xyouts,!point1+(!point2-!point1)/3,ave1_2+(yrangetemp[1]-yrangetemp[0])/60,strtrim(ave1_2,2),charthick=1.5,size=1.6

      mid=!point1+(!point2-!point1)/2.0
      xmid=round(mid/(output_step*tstep))
      num1_mid=xmid-point1+1
      nummid_2=point2-xmid+1
      ave_front=total(y(point1:xmid))/num1_mid
      ave_back=total(y(xmid:point2))/nummid_2
      yave_front=fltarr(2)
      yave_front[*]=ave_front
      yave_back=fltarr(2)
      yave_back[*]=ave_back
      if ave_front ge ave_back then sign=1
      if ave_front le ave_back then sign=-1
     !p.color=color_value(ncolor*1/8) ;green
     cgplot,[!point1,mid],yave_front,thick=1.0,Color='green',/overplot
   xyouts,!point1,yave_front+sign*(yrangetemp[1]-yrangetemp[0])/15,strtrim(ave_front,2),charthick=1.5,size=1.5
      !p.color=color_value(ncolor*1/8)
     cgplot,[mid,!point2],yave_back ,thick=1.0,Color='green',/overplot
   xyouts,mid,yave_back-sign*(yrangetemp[1]-yrangetemp[0])/15,strtrim(ave_back,2),charthick=1.5,size=1.5
     !p.color=color_value(ncolor+1)  ;black


      point3=round(!point3/(output_step*tstep))
      point4=round(!point4/(output_step*tstep))
      num3_4=point4-point3+1
      ave3_4=total(y(point3:point4))/num3_4
      yave3_4=fltarr(2)
      yave3_4[*]=ave3_4
   !p.color=color_value(ncolor*11/16) ;red
   cgplot,[!point3,!point4],yave3_4,thick=1.5,Color='red',/overplot
   xyouts,!point3+(!point4-!point3)/3,ave3_4+(yrangetemp[1]-yrangetemp[0])/60,strtrim(ave3_4,2),charthick=1.5,size=1.6

      mid=!point3+(!point4-!point3)/2.0
      xmid=round(mid/(output_step*tstep))
      num3_mid=xmid-point3+1
      nummid_4=point4-xmid+1
      ave_front=total(y(point3:xmid))/num3_mid
      ave_back=total(y(xmid:point4))/nummid_4
      yave_front=fltarr(2)
      yave_front[*]=ave_front
      yave_back=fltarr(2)
      yave_back[*]=ave_back
      if ave_front ge ave_back then sign=1
      if ave_front le ave_back then sign=-1
     !p.color=color_value(ncolor*1/8) ;green
     cgplot,[!point3,mid],yave_front,thick=1.0,Color='green',/overplot
   xyouts,!point3,yave_front+sign*(yrangetemp[1]-yrangetemp[0])/15,strtrim(ave_front,2),charthick=1.5,size=1.5
      !p.color=color_value(ncolor*1/8)
     cgplot,[mid,!point4],yave_back ,thick=1.0,Color='green',/overplot
   xyouts,mid,yave_back-sign*(yrangetemp[1]-yrangetemp[0])/15,strtrim(ave_back,2),charthick=1.5,size=1.5
     !p.color=color_value(ncolor+1)  ;black

  ;;calculate the intermetancy
  delta_Sat1=sqrt(total((y(point1:point2)-ave1_2)^2)/num1_2 )
  Intermittency1=delta_Sat1/ave1_2

  print, ' from',!point1,'s to',!point2,'s   ave1_2=',ave1_2
  print, ' from',!point1,'s to',!point2,'s Intermittency1=', Intermittency1
  print,' '

  !p.color=color_value(ncolor+1) ;black
  xyouts,xmin+xmax/100,ymax-(yrangetemp[1]-yrangetemp[0])/15,'Intermittency1='+strtrim(Intermittency1,2),charthick=1.2,size=1.2


  delta_Sat2=sqrt(total((y(point3:point4)-ave3_4)^2)/num3_4 )
  Intermittency2=delta_Sat2/ave3_4

  print, ' from',!point3,'s to',!point4,'s   ave3_4=',ave3_4
  print, ' from',!point3,'s to',!point4,'s Intermittency2=', Intermittency2
  print,' '

  !p.color=color_value(ncolor+1) ;black
  xyouts,xmin+xmax/100,ymax-(yrangetemp[1]-yrangetemp[0])/10,'Intermittency2='+strtrim(Intermittency2,2),charthick=1.2,size=1.2

;;----------------------------------------------------------------------------

      endif else begin
  ;; Diffusion PDF histogram

      nbin=15  ;set default value
      it1=max(x)/2.0+ix1
      it2=max(x)

     pdf_statistics,x,y,it1,it2,tstep,output_step;,y_bin,pdf

     y_bin_plot = fltarr(2*nbin)
     pdf_plot   = fltarr(2*nbin)

     i  = indgen(nbin)
     dy = y_bin[1]-y_bin[0]

     y_bin_plot[2*i]   = y_bin[i]-0.5*dy
     y_bin_plot[2*i+1] = y_bin[i]+0.5*dy
     pdf_plot[2*i]     = pdf[i]
     pdf_plot[2*i+1]   = pdf[i]

     xmin = min(y_bin_plot)
     xmax = max(y_bin_plot)
     ymin = min(pdf_plot)
     ymax = max(pdf_plot)

     yave = total(y_bin[*]*pdf[*])
  if logtemp eq 1 then  plot,[0],[0],$
       /nodata,$
;       title=title,$
       xstyle=1,$
       xminor=0,$
       xrange=[xmin,xmax],$
       xtitle=ytitle,$
       ystyle=1,$
       yminor=0,$
       yrange=[ymin,ymax],$
       ytitle='!3Probability Density Function (saturated state)', $
       color=line,$
       /ylog

   if logtemp eq 0 then  plot,[0],[0],$
       /nodata,$
;       title=title,$
       xstyle=1,$
       xminor=0,$
       xrange=[xmin,xmax],$
       xtitle=ytitle,$
       ystyle=1,$
       yminor=0,$
       yrange=[ymin,ymax],$
       ytitle='!3Probability Density Function (saturated state)', $
       color=line


     oplot,y_bin_plot,pdf_plot;,color=color_vec[0]
       !p.color=color_value(ncolor*13/16)
     oplot,yave*[1,1],100*[-1,1],linestyle=1,thick=1.5
     xyouts,yave-(xmax-xmin)/4.0,(ymin+3*ymax)/4.0,'average value of saturate state',size=1.5
  endelse
Endif


  return
end
;*******************************************************************************

pro overplot,x,y,x_title,y_title,xmax,xmin,ymax,ymin,nbegin,logtemp,linestylemum,i,i_begin,zoomtemp,yrange_style,col_sty

  common startup,number_plot,fpath,ncolor,color_value,plotid

;  xmax=max(x)
;  xmin=min(x)
;  ymax=max(y)
;  ymin=min(y)

;  if !D.name eq 'X' then wset,plotid
  IF (i EQ i_begin) THEN BEGIN
  !noeras=0
  !p.color=color_value(ncolor+1)
  set_viewport,0.2,0.95,0.2,0.9
  IF(ymax eq ymin) THEN BEGIN
  ymin=ymin/2
  ymax=ymax*2
  Endif
  set_xy,xmin,xmax,ymin,ymax

   if yrange_style ge 3  then begin
   yrange_style=yrange_style-3
   endif
   if yrange_style eq 1  then begin  ;;plot the middle part of the data
   yrangetemp=[ymin*1.2/zoomtemp,ymax*1.2/zoomtemp]   
   endif
   if yrange_style eq 2 then begin  ;;plot from the min one of the data
   yrangetemp=[-ymax*1.2/zoomtemp,ymax*1.2/zoomtemp]
   endif
   if yrange_style eq 0 then begin  ;;plot upto the max one of the data
   yrangetemp=[0,ymax*1.2/zoomtemp]   
   endif

   ;yrangetemp=[0,0.6]
   
    if logtemp eq 0 then cgplot,[0],[0],$
    /nodata,$
    xrange=[xmin,xmax],$
    yrange=yrangetemp,$
    ;yrange=[0,1.0],$    
    xtitle=x_title,$
    ytitle=y_title,$
    title=!mtitle,$
    linestyle=linestylemum,$
    charsize=1.8

    if logtemp eq 1 then cgplot,[0],[0],$
    /nodata,$
    xrange=[xmin,xmax],$
    yrange=yrangetemp,$
    xtitle=x_title,$
    ytitle=y_title,$
    title=!mtitle,$
    linestyle=linestylemum,$
    charsize=1.8,$
    /ylog

    if logtemp eq 2 then cgplot,[0],[0],$
    /nodata,$
    xrange=[xmin,xmax],$
    yrange=yrangetemp,$
    xtitle=x_title,$
    ytitle=y_title,$
    title=!mtitle,$
    linestyle=linestylemum,$
    charsize=1.8,$
    /xlog,$
    /ylog
 
  Endif
  
;  if logtemp eq 1 then oplot,x,y,linestyle=linestylemum
;  if logtemp eq 0 then oplot,x,y,linestyle=linestylemum

IF(col_sty eq 0)then begin   ;;col_sty=0 -- no color
IF(linestylemum LE 5)THEN BEGIN
  oplot,x,y,linestyle=linestylemum,thick=2.5
ENDif ELSE IF ((linestylemum GT 5) && (linestylemum LE 11))THEN BEGIN
linestylemum=linestylemum-6
  oplot,x,y,linestyle=linestylemum,Psym=-2,thick=2.5
ENDif ELSE IF ((linestylemum GT 11) && (linestylemum LE 17))THEN BEGIN
linestylemum=linestylemum-12
  oplot,x,y,linestyle=linestylemum,Psym=-5,thick=2.5
Endif
EndIF

;oplot,[xmin,xmax],[0,0],thick=1


  return
end
;********************************************************************************

pro spectrum,x,yy,nbegin,ntime,plotidh

  common startup,number_plot,fpath,ncolor,color_value

  if !D.name eq 'X' then wset,plotidh
  !p.color=color_value(ncolor+1)

  ; panel 1: mode history of real and imaginary components
  yr=yy(*,0)
  yi=yy(*,1)
  xmax=max(x)
  xmin=min(x)
  ymax=max([yr,yi])
  ymin=min([yr,yi])

  !noeras=0
  !linetype=0
  set_viewport,0.14,0.54,0.55,0.95
  set_xy,xmin,xmax,ymin,ymax
  plot,x,yr
  xyouts,0.1,0.9,charsize=2.0,'real',/normal

  !p.color=color_value(ncolor*3/4)
  oplot,x,yi
  xyouts,0.1,0.86,charsize=2.0,'imaginary',/normal

  ; panel 2: mode amplitude history
  ya=sqrt(yr*yr+yi*yi)
  ymax=max(ya)
  ymin=min(ya)

  !noeras=1
  !p.color=color_value(ncolor+1)
  !mtitle="mode amplitude .vs. t"
  set_viewport,0.59,0.99,0.55,0.95
  set_xy,xmin,xmax,ymin,ymax
  plot,x,ya,/ylog

  xpeak=fltarr(11)
  ypeak=fltarr(11)
  npeak=0

  for i=nbegin+1,ntime-2 do begin
    if ya(i) gt ya(i-1) and ya(i) gt ya(i+1) then begin
      xpeak(npeak)=i
      ypeak(npeak)=ya(i)
      npeak=min([10,npeak+1])
    end
  end
  npeak=min([9,npeak])
  x1=x(xpeak(0))
  x2=x(xpeak(npeak-1))
  y1=ypeak(0)
  y2=ypeak(npeak-1)
  !p.color=color_value(ncolor*3/4)
  oplot,[x1,x2],[y1,y2]

  gamma=alog(y2/y1)/(x2-x1)
  print,'real frequency=',(npeak-1)*3.14159265/(x2-x1),"    growth rate=",gamma

  ;dispersion relation of plasma oscillation from Brunner
  ;k*lambda_D  omega_r   omega_i
  ; print,'theoretical (k=0.2)=1.06398447, -0.00005523'
  ; print,'theoretical (k=0.3)=1.15984650, -0.01262042'
  print,'theoretical (k=0.4)=1.28505698, -0.06612800'
  ; print,'theoretical (k=0.5)=1.41566189, -0.15335949'
  ; print,'theoretical (k=0.6)=1.54570677, -0.26411036'
  ; print,'theoretical (k=0.7)=1.67386598, -0.39240143'
  ; print,'theoretical (k=0.8)=1.79989932, -0.53455237'
  ; print,'theoretical (k=0.9)=1.92386517, -0.68810933'
  ; print,'theoretical (k=1.0)=2.04590486, -0.85133046'

  ; panel 3: mode amplitude normalized by growth rate
  ; gamma=0.0
  yr=yr/exp(gamma*x)
  yi=yi/exp(gamma*x)
  ysize=size(yr)
  mean=total(yr)/ysize(1)
  yr=yr-mean
  ymin=min([yr,yi])
  ymax=max([yr,yi])

  !noeras=1
  !mtitle="mode"
  !p.color=color_value(ncolor+1)
  set_viewport,0.14,0.54,0.05,0.45
  set_xy,xmin,xmax,ymin,ymax
  plot,x,yr
  !p.color=color_value(ncolor*3/4)
  oplot,x,yi

  ; panel 4: power spectrum
  np=(ntime-nbegin)/16
  power=complex(yr,yi)
  power=fft(power,-1)
  ypow=abs(power)
  yp=fltarr(2*np)
  xp=fltarr(2*np)
  for i=0,np-2 do begin
    yp(i)=ypow(i+ntime-nbegin-np+1)
    xp(i)=(i-np+1)*6.283185/(x(ntime)-x(nbegin))
  end
  for i=0, np do begin
    yp(np-1+i)=ypow(i)
    xp(np-1+i)=i*6.283185/(x(ntime)-x(nbegin))
  end
  xmax=max(xp)
  xmin=min(xp)
  ymax=max(yp)
  ymin=min(yp)

  !noeras=1
  !p.color=color_value(ncolor+1)
  !mtitle="frequency spectrum"
  set_viewport,0.59,0.99,0.05,0.45
  set_xy,xmin,xmax,ymin,ymax
  plot,xp,yp,/xstyle

  ; plasma frequency for k=0.5
  k=0.4
  freqr=sqrt(1.0+3.0*k*k)
  freqi=sqrt(3.14159265/2)*freqr/(k*k*k)*exp(-freqr*freqr/(2.0*k*k))

  ; set display back to terminal after printing to PS file
  if !D.name eq 'PS' then begin
    device,/close
    set_plot,'X'
    !p.thick=1
  endif

  return
end
;********************************************************************************
pro plot3D_mu_time_see,nametemp,windownum,group=group

;  common GLOBAL

  ;;-----------------------------------------------
  ;; Private (local) data
  ;;
  common plotaxis,zoomz,zoombackup0,zoombackup1,log,half,freq,i_ptype,satdelta_t,name,yrange_style
  common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark

  ;;-----------------------------------------------

  ;;------------------------------------------
  ;; Return conditions
  ;;
 ; if exists_diff eq 0 then return
 ; if xregistered('diffusion_ave_see') then return
  ;;------------------------------------------
  name=nametemp
  base = widget_base(title=name[0],$
                     /column)
   ;;set default value
  i_ptype=0
  log=0
  zoomz = 1.0  ;zoomz is the parameter which used to adjust the range of the axis
  satdelta_t=0.0  ;satdelta_t is the parameter which used to adjust the saturation time when the user click the button "t+" or "t-"
  yrange_style=1  ;yrange_style is the parameter which used to choose plot the top, middle or bottom part of the picture.
                  ;for the value of yrange_style/3, 0 for top, 1 for middle, 2 for bottom;
  defsysv,'!Azdelta',.0 ;;to rotate the z axis
  defsysv,'!Axdelta',.0 ;;to rotate the x axis
  defsysv,'!jumpmul',1.0 ;;to modify the jump  ;; Must get float initial value, like 1.0 !
  defsysv,'!zmintem',1.0 ;;to modify the jump  !zmintem  ;; Must get float initial value, like 1.0 !
  defsysv,'!twoD',0  ;;decide plot 2D or not
  
  ;i_tp = 0
  ;;----------------------------------------------------------
  ;; BUTTONS
  ;;----------------------------------------------------------

  row1 = widget_base(base,$
                     /row,$
                     /frame)

  x = widget_button(row1, $
                    value='Plot', $
                    uvalue=0)

  x = widget_button(row1, $
                    value='t+', $
                    uvalue=1)

  x = widget_button(row1, $
                    value='t-', $
                    uvalue=2)

  x = widget_button(row1, $
                    value='Z+', $
                    uvalue=3)

  x = widget_button(row1, $
                    value='Z-', $
                    uvalue=4)

  x = widget_button(row1, $
                    value='X+', $
                    uvalue=5)

  x = widget_button(row1, $
                    value='X-', $
                    uvalue=6)

  x = widget_button(row1, $
                    value='zmin*10', $
                    uvalue=7)

  x = widget_button(row1, $
                    value='zmin/10', $
                    uvalue=8)

  x = widget_button(row1, $
                    value='jump*2', $
                    uvalue=9)

  x = widget_button(row1, $
                    value='jump/2', $
                    uvalue=10)

  x = widget_button(row1,$
                    value='log',$
                    uvalue=11)

  x = widget_button(row1, $
                    value='2D', $
                    uvalue=12)
                    
  x = widget_button(row1, $
                    value='Done', $
                    uvalue=13)
  ;;----------------------------------------------------------
  ;; DRAW WIDGET and CONTROL
  ;;----------------------------------------------------------

  draw = widget_draw(base,     $
                     xsize=600, $
                     ysize=650)

  widget_control, base, $
    ;set_uvalue=state,$
    /no_copy, $
    /realize

  ;!plotkspecid=!D.WINDOW
    (*windowmark)[windownum]=!D.window
    basemark[windownum]=base

  xmanager,'plot3D_mu_time', $
    base,$
;    event='energy_time_trace_event',$
    group_leader=group


end

;*******************************************************************************
pro plot3D_mu_time_event,event

  common startup,number_plot,fpath,ncolor,color_value,plotid
  common plotaxis,zoomz,zoombackup0,zoombackup1,log,half,freq,i_ptype,satdelta_t,name,yrange_style
  common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark

  widget_control, event.id, $
    get_uvalue=uvalue
 ; wset, widget

openr,lun,'inputRCYCLO.txt',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_variables',plot_name_temp,19)
  endwhile
  thisFormat='(8x,i2,/,9x,i2,/,4x,i2)'
  readf,lun,restart,GK_FK_CK,CDW,Format=thisFormat
  thisFormat='(8x,i2,/,11x,i2,/,12x,i5,/,11x,i4,/,9x,f5.3,/,/,/,/,7x,i10)'
  readf,lun,muDtype,mugridtype,output_step,backup_num,Const_nl,Stopnt,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Grid_variables',plot_name_temp,16)
  endwhile
  thisFormat='(6x,i10,/,6x,f12.9,/,6x,f10.3,/,6x,f10.3,/,5x,i5,/,5x,i5,/,6x,i8.3,/,5x,i5,/,5x,i6,/,5x,i6)'
  readf,lun,ntmax,tstep,kxmax,kymax,pmax,nmax,mumax,N_mu,$
      N_FT,N_CY,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Physics_variables',plot_name_temp,19)
  endwhile
  thisFormat='(11x,f7.4,/,5x,f5.3,/,6x,f6.3,/,9x,f5.3,/,9x,f5.3,/,9x,f5.3,/,7x,f9.3,/,8x,f5.3,/,6x,f5.3,/,6x,f5.3,/,8x,f12.7,/,8x,f8.5,/,/,/,6x,f8.4)'
  readf,lun,Omega_star,a_Ln,a_LTi,lambda_n,lambda_0,lambda_D,AlphaA,delta_1,$
  mu_HK,mu_LK,F_k_int,Epsilon,gamIC, Format=thisFormat

free_lun,lun



openr,lun,'vuM6D.input',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_variables',plot_name_temp,19)
  endwhile
  thisFormat='(7x,i5,/,9x,i5,/,8x,f10.3,/,14x,i2,/,13x,i2,/,3x,i3,/,3x,i3,/,4x,i6,/,4x,i6,/,11x,i4)'
  readf,lun,avenum,plotstep,sattime,Intermit_Plot,overplot_log,Az,Ax,ix1,ix2,Switch_sat,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Range_control',plot_name_temp,15)
  endwhile
  thisFormat='(10x,f15.12,/,10x,f12.4,/,11x,f15.10,/,11x,f15.12,/,9x,i5,/,8x,f10.4)'
  readf,lun,Omega_min,Omega_max,Phi_logmin,Chi_logmin,OMGpoint,freqPer,Format=thisFormat

free_lun,lun



;  openr,lun,'nt_Odd.txt',/get_lun
;  readf,lun,nt_Odd
;  free_lun,lun
;  openr,lun,'nt_Even.txt',/get_lun
;  readf,lun,nt_Even
;  free_lun,lun
;  IF(ntmax GE max([nt_Odd,nt_Even]))Then Begin
;  ntmax=max([nt_Odd,nt_Even])
;  Endif
;  print,'ntmax=',ntmax

  Time=findgen(ntmax/output_step+1)*(tstep*output_step)
  kx=findgen(2*pmax-1)*kxmax/(pmax-1)-kxmax
  ky=findgen(2*nmax-1)*kymax/(nmax-1)-kymax
  kxhalf=findgen(pmax)*kxmax/(pmax-1)
  kyhalf=findgen(nmax)*kymax/(nmax-1)
  mu=fltarr(N_mu)


  For i=0,ndata-1 Do begin
  IF(basemark[i] eq event.top)then begin
    IF((*windowmark)[i] ne !D.window)then begin
    wset,(*windowmark)[i]
    Endif
    case(i) of
    39: name=[cgGreek('chi')+'i (GK)','Chi_mu.txt','cpu_timeGK.txt']
    40: name=[cgGreek('chi')+'i (CKinFH)','Chi_muft.txt','cpu_timeFK.txt']
    41: name=['F (GK)','Fk_mu.txt','cpu_timeGK.txt']
    42: name=['F (CKinFH)','Fk_ftmu.txt','cpu_timeFK.txt']
    43: name=['F_tot (CKinFH)','Fk_ftmuT.txt','cpu_timeFK.txt']
    endcase
  Endif
  Endfor

  openr,lun,name[2],/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp(' mu_point=',plot_name_temp,10)
  endwhile
  readf,lun,mu
  free_lun,lun
  
  case (uvalue) of

     0:begin
     goto, plot_it
     end

     1:begin
        satdelta_t = satdelta_t+(ntmax/40)*tstep  ;satdelta_t is the variable which used to adjust the saturation time when the user click the button "t+" or "t-"
        if (satdelta_t ge (ntmax)*tstep-sattime) then satdelta_t = 0
        goto, plot_it
     goto, plot_it
     end

     2: begin
        satdelta_t = satdelta_t-(ntmax/40)*tstep
        if (satdelta_t le -sattime) then satdelta_t = 0
        goto, plot_it
     end

     3: begin
        !Azdelta=!Azdelta+10
        goto, plot_it
     end

     4: begin
        !Azdelta=!Azdelta-10
        goto, plot_it
     end

     5: begin
        !Axdelta=!Axdelta+5
        goto, plot_it
     end

     6: begin
        !Axdelta=!Axdelta-5
        goto, plot_it
     end

     7: begin
        !zmintem=!zmintem*10.0
        goto, plot_it
     end

     8: begin
        !zmintem=!zmintem/10.0
        goto, plot_it
     end

     9: begin
        !jumpmul=!jumpmul*2.0
        goto, plot_it
     end

     10: begin
        !jumpmul=!jumpmul/2.0
        goto, plot_it
     end

     11: begin
        log = (log+1) mod 2
        goto, plot_it
     end

     12: begin ;shut down this window
      !twoD=(!twoD+1) mod 2
      goto, plot_it
     end

     13: begin ;shut down this window
;     IF(!plotwindowsid eq 10)then begin
;     Wdelete,!plotwindowsid
;     !plotwindowsid=0
;     Endif
     widget_control, event.top, /destroy
     end
     
  endcase

  return

  plot_it:


  ntnum_of_bin=round(ntmax/(output_step*avenum))
  !p.background=color_value(ncolor/2)
;=================================================================================================================
;      plotidh=number_plot+1
;      !p.background=color_value(ncolor/2)
;      window, plotidh, TITLE='main_plot', xsize=700,ysize=600

      jump=1*!jumpmul
      IF(ntmax/output_step GE 1000)then begin
      jump=100*!jumpmul
      Endif Else IF(ntmax/output_step GE 10000)then begin
      jump=1000*!jumpmul
      Endif Else IF(ntmax/output_step GE 100000)then begin
      jump=10000*!jumpmul
      Endif Else IF(ntmax/output_step GE 1000000)then begin
      jump=100000*!jumpmul
      Endif

      Z=fltarr(N_mu,ntmax/(output_step*jump)+1)
      temp=fltarr(N_mu)

      openr,lun,name[1],/get_lun
      readf,lun,temp
      POINT_LUN, -lun, pos
      Print,'Pos=',pos
      For i=0L,ntmax/output_step,jump Do begin
      POINT_LUN, lun, pos*i
      readf,lun,temp
      Z(*,i/jump)=temp
      Endfor
      free_lun,lun

      sattime=sattime+satdelta_t
      Az=Az+!Azdelta
      Ax=Ax+!Axdelta
      Chi_logmin=Chi_logmin*!zmintem

      x=mu
      y=findgen(ntmax/(output_step*jump)+1)*(tstep*output_step*jump)
      !mtitle=name[0]+' '+strtrim(round(sattime),2)+'-'+strtrim(round(ntmax*tstep),2)+'Ln/Cs'
      surface3D,Z(*,round(sattime/(output_step*jump*tstep)):round(ntmax/(output_step*jump))),x,$
      y(round(sattime/(output_step*jump*tstep)):round(ntmax/(output_step*jump))),cgGreek('mu'),'time',0,1,log,Az,Ax,Chi_logmin

      if(!twoD eq 1)then begin

     !p.background=color_value(ncolor/2)
      window, plotidh+2, TITLE='main_plot', xsize=500,ysize=550
      chimu=fltarr(N_mu)
      chimu=total(Z(*,round(sattime/(output_step*jump*tstep)):round(ntmax/(output_step*jump))),2)/round((ntmax-sattime+1)/(output_step*jump))
      plot,x,chimu,$
      xtitle=cgGreek('mu'),$
      xrange=[min(x),max(x)],$
      yrange=[min(chimu),1.2*max(chimu)],$
       ytitle=name[0], $
       title=!mtitle
      endif
;=================================================================================================================

end
;********************************************************************************
pro Main_plots_name

  common startup,number_plot,fpath,ncolor,color_value,plotid
  common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark

  defsysv,'!plotkspecid',4   ;;define systerm variable to count the plot times of E(kx) and D(kx)...
  defsysv,'!plotwindowsid',0   ;;defined to mark the windows number

;  ; default window
  plotidh=number_plot
   !p.background=color_value(ncolor/2)
;  window, plotidh, TITLE='main_plot', xsize=600,ysize=600

  openr, plotidh, 'vuM6D.input'
  ; # of time steps and # of data
  ntime=1
  ndata=1
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,plotidh,plot_name_temp
  result1=strcmp('##Main_plots_name',plot_name_temp,17)
  endwhile
  readf,plotidh,ntime,ndata

  ; names of data
  data_name=strarr(ndata)
  readf,plotidh,data_name
  close, plotidh


  ; widget panel
  hname=strarr(ndata)
  hname(0:ndata-1)=data_name

  windowmark=ptr_new(intarr(200)) ;;create a point array to record the ID of windows.
  basemark=intarr(200)

  xmenu,hname,BASE=hbase,SPACE=10,TITLE='inputvuRCYCLO',column=1,xpad=20,ypad=20
  widget_control,hbase,/realize
  xmanager,"Main_plots",hbase,/NO_BLOCK


end

;*******************************************************************************
pro colorful_plot_name

  common startup,number_plot,fpath,ncolor,color_value,plotid
  common colorful_plot,Colndata,Colwindowmark,Colbasemark

;  ; default window
  lun=number_plot
   !p.background=color_value(ncolor/2)

  openr, lun, 'vuM6D.input',/get_lun
  ; # of time steps and # of data
  Colndata=1
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##colorful_plot_name',plot_name_temp,20)
  endwhile
  readf,lun,Colndata

  ; names of data
  data_name=strarr(Colndata)
  readf,lun,data_name
  free_lun,lun

  ; widget panel
  hname=strarr(Colndata)
  hname(0:Colndata-1)=data_name

  Colwindowmark=ptr_new(intarr(Colndata)) ;;create a point array to record the ID of windows.
  Colbasemark=intarr(Colndata)

  xmenu,hname,BASE=hbase,SPACE=10,TITLE='colorful_plot',column=2,xpad=20,ypad=20
  widget_control,hbase,/realize
  xmanager,"colorful_plot",hbase,/NO_BLOCK


end

;*******************************************************************************

pro colorful_Plot_event,event

  common startup,number_plot,fpath,ncolor,color_value,plotid
  common colorful_plot,Colndata,Colwindowmark,Colbasemark

  widget_control,event.id,get_uvalue=choice

  !p.thick=2

  case choice of

    0: begin
      ;plotidh=1
      ;wdelete,plotidh        ; Closes plot windows
     ; plotidh=!D.window
      while(!D.window NE -1) Do begin
    ;  IF( plotidh GT 0)then Begin
      Wdelete,!D.window  ;;delete all the windows.
     ; plotidh=plotidh-1
      Endwhile
      widget_control, event.top,/destroy
    end

    1: begin ;total energy
    nametemp=['Energy(k)','energy','energy_k_sat.txt']
    ;;nametemp include: title, ytitle, and input data file name
     Colorfulplot_see,nametemp,1
    end

    2: begin ;total energy
    nametemp=['Omega_sim(k)','Omega','Phi_k.txt']
    Colorfulplot_see,nametemp,2
    end

    3: begin ;total energy
    nametemp=['Omega_the(k)','Omega','Omega_the.txt']
    Colorfulplot_see,nametemp,3
    end

    4: begin ;total energy
    nametemp=['Omega_matr(k)','Omega','Omega_matr.txt']
    Colorfulplot_see,nametemp,4
    end

    5: begin ;eddy: phi vs (x,y)
    nametemp=['|ne|','|ne|','ne_k.txt']
    Colorfulplot_see,nametemp,5
    end
    
    6: begin ;eddy: phi vs (x,y)
    nametemp=['|Phi|^2','|Phi|^2','Phi_kOMG.txt']
    Colorfulplot_see,nametemp,6
    end
    
  endcase
end

;*******************************************************************************

pro Colorfulplot_see,nametemp,windownum,group=group

;  common GLOBAL

  ;;-----------------------------------------------
  ;; Private (local) data
  ;;
;  common PRIVATE_time_trace,widget
  common plotaxis,zoomz,zoombackup0,zoombackup1,log,half,freq,i_ptype,satdelta_t,name,yrange_style
  ;common main_plot,plotidh,ntime,ndata,data_name,windowmark,basemark
  common colorful_plot,Colndata,Colwindowmark,Colbasemark

  ;;-----------------------------------------------

  ;;------------------------------------------
  ;; Return conditions
  ;;
 ; if exists_diff eq 0 then return
 ; if xregistered('diffusion_ave_see') then return
  ;;------------------------------------------
  name=nametemp
  base = widget_base(title=name[0],$
                     /column)
   ;;set default value
  defsysv,'!Fignum',1
  defsysv,'!Fignum2',1
  defsysv,'!Omegap',0
  defsysv,'!velocity',0  ;0 for no velocity vector plot, >0 for ploting velocity vector at a certain moment.
  i_ptype=0
  log=0
  zoomz = 1.0  ;zoomz is the parameter which used to adjust the range of the axis
  satdelta_t=0.0  ;satdelta_t is the parameter which used to adjust the saturation time when the user click the button "t+" or "t-"
  yrange_style=1  ;yrange_style is the parameter which used to choose plot the top, middle or bottom part of the picture.
                  ;for the value of yrange_style/3, 0 for top, 1 for middle, 2 for bottom;
  ;i_tp = 0
  ;;----------------------------------------------------------
  ;; BUTTONS
  ;;----------------------------------------------------------

  row1 = widget_base(base,$
                     /row,$
                     /frame)

  IF(strcmp(name[0],'Omega_',6)) THEN BEGIN
  x = widget_button(row1, $
                    value='Plot(re)', $
                    uvalue=0)

  x = widget_button(row1, $
                    value='Plot(im)', $
                    uvalue=1)
  x = widget_button(row1,$
                    value='Type',$
                    uvalue=2)
  Endif Else Begin
  x = widget_button(row1, $
                    value='Plot', $
                    uvalue=0)
                    
  x = widget_button(row1,$
                    value='log',$
                    uvalue=7)    

  x = widget_button(row1,$
                    value='velocity',$
                    uvalue=3)                                       
  Endelse
;  x = widget_button(row1, $
;                    value='t+', $
;                    uvalue=1)
;
;  x = widget_button(row1, $
;                    value='t-', $
;                    uvalue=2)
;
;  x = widget_button(row1, $
;                    value='ZOOM in', $
;                    uvalue=3)
;
;  x = widget_button(row1, $
;                    value='ZOOM out', $
;                    uvalue=4)
;  x = widget_button(row1, $
;                    value='Yrange', $
;                    uvalue=5)
;  x = widget_button(row1, $
;                    value='units', $
;                    uvalue=6)
;
;  x = widget_button(row1,$
;                    value='log',$
;                    uvalue=7)
;
;  x = widget_button(row1,$
;                    value='TYPE',$
;                    /menu)
;
;  tlevels=['Line Plot','PDF']
;  for i=0,1 do begin
;     x1 = widget_button(x,$
;                        value=tlevels[i],$
;                        uvalue=20+i)
;  endfor
;
;  x = widget_button(row1, $
;                    value='Cal E(k)sat', $
;                    uvalue=9)

  x = widget_button(row1, $
                    value='Done', $
                    uvalue=8)

  ;;----------------------------------------------------------
  ;; DRAW WIDGET and CONTROL
  ;;----------------------------------------------------------

  draw = widget_draw(base,     $
                     xsize=600, $
                     ysize=500)

  widget_control, base, $
    ;set_uvalue=state,$
    /no_copy, $
    /realize

  ;!plotkspecid=!D.WINDOW
    (*colwindowmark)[windownum]=!D.window
    colbasemark[windownum]=base

  xmanager,'Colorfulplot', $
    base,$
;    event='energy_time_trace_event',$
    group_leader=group


end

;*******************************************************************************

pro Colorfulplot_event,event

  common startup,number_plot,fpath,ncolor,color_value,plotid
  common plotaxis,zoomz,zoombackup0,zoombackup1,log,half,freq,i_ptype,satdelta_t,name,yrange_style
  common colorful_plot,Colndata,Colwindowmark,Colbasemark

  widget_control, event.id, $
    get_uvalue=uvalue
 ; wset, widget

openr,lun,'inputRCYCLO.txt',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_variables',plot_name_temp,19)
  endwhile
  thisFormat='(8x,i2,/,9x,i2,/,4x,i2)'
  readf,lun,restart,GK_FK_CK,CDW,Format=thisFormat
  thisFormat='(8x,i2,/,11x,i2,/,12x,i5,/,11x,i4,/,9x,f5.3,/,/,/,/,7x,i10)'
  readf,lun,muDtype,mugridtype,output_step,backup_num,Const_nl,Stopnt,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Grid_variables',plot_name_temp,16)
  endwhile
  thisFormat='(6x,i10,/,6x,f12.9,/,6x,f10.3,/,6x,f10.3,/,5x,i5,/,5x,i5,/,6x,i8.3,/,5x,i5,/,5x,i6,/,5x,i6)'
  readf,lun,ntmax,tstep,kxmax,kymax,pmax,nmax,mumax,N_mu,$
      N_FT,N_CY,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Physics_variables',plot_name_temp,19)
  endwhile
  thisFormat='(11x,f7.4,/,5x,f5.3,/,6x,f6.3,/,9x,f5.3,/,9x,f5.3,/,9x,f5.3,/,7x,f9.3,/,8x,f5.3,/,6x,f5.3,/,6x,f5.3,/,8x,f12.7,/,8x,f8.5,/,/,/,6x,f8.4)'
  readf,lun,Omega_star,a_Ln,a_LTi,lambda_n,lambda_0,lambda_D,AlphaA,delta_1,$
  mu_HK,mu_LK,F_k_int,Epsilon,gamIC, Format=thisFormat

free_lun,lun

N_mu=N_mu+1

openr,lun,'vuM6D.input',/get_lun
  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Control_variables',plot_name_temp,19)
  endwhile
  thisFormat='(7x,i5,/,9x,i5,/,8x,f10.3,/,14x,i2,/,13x,i2,/,/,/,4x,i6,/,4x,i6,/,11x,i4)'
  readf,lun,avenum,plotstep,sattime,Intermit_Plot,overplot_log,ix1,ix2,Switch_sat,Format=thisFormat

  plot_name_temp="aaa"
  result1=0
  While (result1 eq 0) Do Begin
  readf,lun,plot_name_temp
  result1=strcmp('##Range_control',plot_name_temp,15)
  endwhile
  thisFormat='(10x,f15.12,/,10x,f12.4,/,11x,f15.10,/,11x,f15.12,/,9x,i5,/,8x,f10.4)'
  readf,lun,Omega_min,Omega_max,Phi_logmin,Chi_logmin,OMGpoint,freqPer,Format=thisFormat

free_lun,lun



;  openr,lun,'nt_Odd.txt',/get_lun
;  readf,lun,nt_Odd
;  free_lun,lun
;  openr,lun,'nt_Even.txt',/get_lun
;  readf,lun,nt_Even
;  free_lun,lun
;  IF(ntmax GE max([nt_Odd,nt_Even]))Then Begin
;  ntmax=max([nt_Odd,nt_Even])
;  Endif
;  print,'ntmax=',ntmax

  Time=findgen(ntmax/output_step+1)*(tstep*output_step)
  kx=findgen(2*pmax-1)*kxmax/(pmax-1)-kxmax
  ky=findgen(2*nmax-1)*kymax/(nmax-1)-kymax
  kxhalf=findgen(pmax)*kxmax/(pmax-1)
  kyhalf=findgen(nmax)*kymax/(nmax-1)

  ;;-------------------------------------------------------
  ;; MENU
  ;;-------------------------------------------------------

  ;;set the click botton action act on the window you want.
  For i=0,colndata-1 Do begin
  IF(colbasemark[i] eq event.top)then begin
    IF((*colwindowmark)[i] ne !D.window)then begin
    wset,(*colwindowmark)[i]
    Endif
    case(i) of
    1: name=['Energy(k)','energy','energy_k_sat.txt']
    2: name=['Omega_sim(k)','Omega','Phi_k.txt']
    3: name=['Omega_the(k)','Omega','Omega_the.txt']
    4: name=['Omega_matr(k)','Omega','Omega_matr.txt']
    5: name=['|ne|','|ne|','ne_k.txt']
    6: name=['|Phi|^2','|Phi|^2','Phi_kOMG.txt'] ;;eddy movie
    endcase
  Endif
  Endfor


  case (uvalue) of

;  IF(strcmp(name[0],'Omega_',6)) THEN BEGIN
     0:begin
     !Omegap=0
     goto, plot_it
     end

     1:begin
     !Omegap=1
     goto, plot_it
     end

     2:begin
     !Fignum=!Fignum+1
     if !Fignum gt 3  then begin
     !Fignum=!Fignum-3
     endif
     !Fignum2=!Fignum2+1
     if !Fignum2 gt 7  then begin
     !Fignum2=!Fignum2-7
     endif
     goto, plot_it
     end

     3:begin
     !velocity=!velocity+1
     goto, plot_it
     end
          
     7: begin
        log = (log+1) mod 2
        goto, plot_it
     end
          
     8: begin ;shut down this window
;     IF(!plotwindowsid eq 10)then begin
;     Wdelete,!plotwindowsid
;     !plotwindowsid=0
;     Endif
     widget_control, event.top, /destroy
     end


  endcase

  return

  plot_it:

  ;;-------------------------------------------------------
  ;; PLOTTING
  ;;-------------------------------------------------------
  neps=0  ;;0 output on screen; 1 for .eps file.
  rms=2   ;?张画不同的图的不同载入方式
  pbar=100  ;画colorbar分多少块来画。
  levs=256  ;假彩色的颜色数量
  contr=1.5 ;画线的粗细度
  contrs=1.5
 device,decomposed=0
 ;;Set this keyword to 0 to cause color values to be interpreted as indices into a color lookup table.
 ;;Set this keyword to 1 to cause color values to be interpreted as 24-bit color specifications.
  loadct,39
  !p.thick=contr
  !x.thick=contr
  !y.thick=contr
  !p.charsize=contrs
  !p.charthick=contr
  !p.background=255
  !p.color=0
  ; posi1=[0.25,0.06,0.85,0.08]
   posi1=[0.9,0.3,0.92,0.9]
   posi2=[0.13,0.3,0.73,0.9]  ;;左下角与右上角两点。

   if rms eq 1 then begin
   loadct,39
     ccol=indgen(levs)/double(levs)*(256)
     ;ccol(0:55)=25    ;用同一种颜色来显示大于（小于）某个值的区域
     ;ccol(200:255)=230 ;用同一种颜色来显示大于（小于）某个值的区域
   endif else begin
     ccol=255-indgen(levs)/double(levs)*(256-25)
     ;ccol(0:25)=255  ;用同一种颜色来显示大于（小于）某个值的区域
   endelse

  zd=fltarr(2*pmax-1,2*nmax-1)

  IF(strcmp(name[0],'Omega_sim(k)',12)) THEN BEGIN
  Omega=complexarr(pmax,nmax)
  Omegacy=complexarr(pmax,nmax)
  Omegaft=complexarr(pmax,nmax)
      Phi_kd=complexarr(2*pmax-1,2*nmax-1,ntmax/output_step+1)
      openr,lun,'Phi_k.txt',/get_lun
      readf,lun,Phi_kd
      free_lun,lun
      Phi_kftd=complexarr(2*pmax-1,2*nmax-1,ntmax/output_step+1)
      openr,lun,'Phi_kft.txt',/get_lun
      readf,lun,Phi_kftd
      free_lun,lun
      Phi_kcyd=complexarr(2*pmax-1,2*nmax-1,ntmax/output_step+1)
      openr,lun,'Phi_kcy.txt',/get_lun
      readf,lun,Phi_kcyd
      free_lun,lun
  Omega(*,*)=complex(0,0)
  Omegacy(*,*)=complex(0,0)
  Omegaft(*,*)=complex(0,0)

      tmax=max(Time)
      xx1=tmax*0.5
      xx2=tmax-tmax/40.0
      xxx1=round(xx1/(output_step*tstep))
      xxx2=round(xx2/(output_step*tstep))
      lxx1=xx1-tmax/40.0
      rxx1=xx1+tmax/40.0
      lxx2=xx2-tmax/40.0
      rxx2=xx2+tmax/40.0
      lxxx1=round(lxx1/(output_step*tstep))
      rxxx1=round(rxx1/(output_step*tstep))
      lxxx2=round(lxx2/(output_step*tstep))
      rxxx2=round(rxx2/(output_step*tstep))
   For p=0,pmax-1 Do Begin
    For n=0,nmax-1 Do Begin
     For i=1,ntmax/output_step Do Begin
     If((phi_kd(p+pmax-1,n+nmax-1,i)+phi_kd(p+pmax-1,n+nmax-1,i-1)) eq complex(0,0))then begin
     Omega(p,n)=Omega(p,n)+complex(0,1)*(phi_kd(p+pmax-1,n+nmax-1,i)-phi_kd(p+pmax-1,n+nmax-1,i-1))/(output_step*tstep)
     Endif Else Begin
     Omega(p,n)=Omega(p,n)+2*complex(0,1)*(phi_kd(p+pmax-1,n+nmax-1,i)-phi_kd(p+pmax-1,n+nmax-1,i-1))/(output_step*tstep*(phi_kd(p+pmax-1,n+nmax-1,i)+phi_kd(p+pmax-1,n+nmax-1,i-1)))
     Endelse
     Endfor
     Omega(p,n)=Omega(p,n)/(ntmax/output_step)

     For i=1,ntmax/output_step Do Begin
     If((phi_kcyd(p+pmax-1,n+nmax-1,i)+phi_kcyd(p+pmax-1,n+nmax-1,i-1)) eq complex(0,0))then begin
     Omegacy(p,n)=Omegacy(p,n)+complex(0,1)*(phi_kcyd(p+pmax-1,n+nmax-1,i)-phi_kcyd(p+pmax-1,n+nmax-1,i-1))/(output_step*tstep)
     Endif Else Begin
     Omegacy(p,n)=Omegacy(p,n)+2*complex(0,1)*(phi_kcyd(p+pmax-1,n+nmax-1,i)-phi_kcyd(p+pmax-1,n+nmax-1,i-1))/(output_step*tstep*(phi_kcyd(p+pmax-1,n+nmax-1,i)+phi_kcyd(p+pmax-1,n+nmax-1,i-1)))
     Endelse
     Endfor
     Omegacy(p,n)=Omegacy(p,n)/(ntmax/output_step)

     For i=1,ntmax/output_step Do Begin
     If((phi_kftd(p+pmax-1,n+nmax-1,i)+phi_kftd(p+pmax-1,n+nmax-1,i-1)) eq complex(0,0))then begin
     Omegaft(p,n)=Omegaft(p,n)+complex(0,1)*(phi_kftd(p+pmax-1,n+nmax-1,i)-phi_kftd(p+pmax-1,n+nmax-1,i-1))/(output_step*tstep)
     Endif Else Begin
     Omegaft(p,n)=Omegaft(p,n)+2*complex(0,1)*(phi_kftd(p+pmax-1,n+nmax-1,i)-phi_kftd(p+pmax-1,n+nmax-1,i-1))/(output_step*tstep*(phi_kftd(p+pmax-1,n+nmax-1,i)+phi_kftd(p+pmax-1,n+nmax-1,i-1)))
     Endelse
     Endfor
     Omegaft(p,n)=Omegaft(p,n)/(ntmax/output_step)

      y1=total(alog(abs(Phi_kd(p+pmax-1,n+nmax-1,lxxx1:rxxx1))))/(rxxx1-lxxx1+1)
      y2=total(alog(abs(Phi_kd(p+pmax-1,n+nmax-1,lxxx2:rxxx2))))/(rxxx2-lxxx2+1)
      y1cy=total(alog(abs(Phi_kcyd(p+pmax-1,n+nmax-1,lxxx1:rxxx1))))/(rxxx1-lxxx1+1)
      y2cy=total(alog(abs(Phi_kcyd(p+pmax-1,n+nmax-1,lxxx2:rxxx2))))/(rxxx2-lxxx2+1)
      y1ft=total(alog(abs(Phi_kftd(p+pmax-1,n+nmax-1,lxxx1:rxxx1))))/(rxxx1-lxxx1+1)
      y2ft=total(alog(abs(Phi_kftd(p+pmax-1,n+nmax-1,lxxx2:rxxx2))))/(rxxx2-lxxx2+1)

      Omega(p,n)=complex(REAL_PART(Omega(p,n)),(y2-y1)/(xx2-xx1))
      Omegacy(p,n)=complex(REAL_PART(Omegacy(p,n)),(y2cy-y1cy)/(xx2-xx1))
      Omegaft(p,n)=complex(REAL_PART(Omegaft(p,n)),(y2ft-y1ft)/(xx2-xx1))
      Omega(0,0)=complex(0,0)
      Omegacy(0,0)=complex(0,0)
      Omegaft(0,0)=complex(0,0)
    Endfor
   Endfor
   
;;----------------------------------------------------------------------
Endif Else IF( strcmp(name[0],'Omega_matr(k)',13) ) THEN BEGIN
  tempReconst=1  ;!!!!!!!!!!!!!!!!
  zdh=fltarr(pmax,nmax)
  Omega_matOrg=complexarr(2*pmax-1,nmax,N_mu)
  Omega_matrFinal=complexarr(pmax,nmax)
  openr,lun,'Omega_matr.txt',/get_lun   ;;0 Gyro-kinetic
  readf,lun,Omega_matOrg
  n=eof(lun)
  if n ne 1 then print,'error with file load!!!!!!'
  free_lun,lun
  tempRe=100
  For p=pmax-1,2*pmax-2 Do Begin
   For n=0,nmax-1 Do Begin
    jtemp=-1
    For j=0,N_mu-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrg(p,n,j))) LE tempRe)THEN BEGIN
          jtemp=j
          growthIm=imaginary(Omega_matOrg(p,n,j))
          Endif
    Endfor

     For j=0,N_mu-1 Do Begin
       IF(abs(REAL_PART(Omega_matOrg(p,n,j))) LE tempRe)THEN BEGIN
       IF(imaginary(Omega_matOrg(p,n,j)) GT growthIm)then begin
       jtemp=j
       growthIm=imaginary(Omega_matOrg(p,n,j))
       Endif
       Endif
     Endfor
     IF(jtemp EQ -1)then begin
     Omega_matrFinal(p-pmax+1,n)=complex(0,0)
     Endif else begin
     Omega_matrFinal(p-pmax+1,n)=Omega_matOrg(p,n,jtemp)
     Endelse
   Endfor
  Endfor

  Omega_matrcyLow=complexarr(pmax,nmax)
  Omega_matrcyHigh=complexarr(pmax,nmax)
  Omega_matOrgcy=complexarr(2*pmax-1,nmax,N_mu*(2*N_CY-1))
  Omega_matOrgcy(*,*,*)=complex(0,0)
  openr,lun,'Omega_matrcy.txt',/get_lun
  readf,lun,Omega_matOrgcy
  n=eof(lun)
  if n ne 1 then print,'error with file load!!!!!!'
  free_lun,lun
  For p=0,pmax-1 Do Begin
   For n=0,nmax-1 Do Begin
  tempRe=abs(REAL_PART(Omega_matOrgcy(p+pmax-1,n,0)))
    For j=0,N_mu*(2*N_CY-1)-1 Do Begin
      IF(tempRe LE abs(REAL_PART(Omega_matOrgcy(p+pmax-1,n,j)))) THEN BEGIN
           tempRe= abs(REAL_PART(Omega_matOrgcy(p+pmax-1,n,j)))
       Endif
    Endfor
    IF(N_CY GE 2)then begin
    tempRe=tempRe/Omega_star*tempReconst/(N_CY-1) ;!!!!!!!!!!!!!!!!!! 1. 2.
    ;tempRe=1
    Endif

    jtemp=-1
    For j=0,N_mu*(2*N_CY-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgcy(p+pmax-1,n,j))) LE tempRe)THEN BEGIN
          jtemp=j
          growthIm=imaginary(Omega_matOrgcy(p+pmax-1,n,j))
          Endif
    Endfor
    For j=0,N_mu*(2*N_CY-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgcy(p+pmax-1,n,j))) LE tempRe)THEN BEGIN
           IF(growthIm LT imaginary(Omega_matOrgcy(p+pmax-1,n,j))) THEN BEGIN
           growthIm= imaginary(Omega_matOrgcy(p+pmax-1,n,j))
           jtemp=j
           Endif
          Endif
    Endfor
    IF(jtemp EQ -1)then begin
    Omega_matrcyLow(p,n)=complex(0,0)
    Endif else begin
    Omega_matrcyLow(p,n)=Omega_matOrgcy(p+pmax-1,n,jtemp)
    Endelse

    jtemp=-1
    For j=0,N_mu*(2*N_CY-1)-1 Do Begin
          IF((abs(REAL_PART(Omega_matOrgcy(p+pmax-1,n,j))) GE tempRe)) THEN BEGIN
          jtemp=j
          growthIm=imaginary(Omega_matOrgcy(p+pmax-1,n,j))
          Endif
    Endfor
    For j=0,N_mu*(2*N_CY-1)-1 Do Begin
          IF((abs(REAL_PART(Omega_matOrgcy(p+pmax-1,n,j))) GE tempRe)) THEN BEGIN
            IF(imaginary(Omega_matOrgcy(p+pmax-1,n,j)) GT growthIm) THEN BEGIN
            jtemp=j
            growthIm=imaginary(Omega_matOrgcy(p+pmax-1,n,j))
            Endif
          Endif
    Endfor
    IF(jtemp EQ -1)then begin
    Omega_matrcyHigh(p,n)=complex(0,0)
    Endif else begin
    Omega_matrcyHigh(p,n)=Omega_matOrgcy(p+pmax-1,n,jtemp)
    Endelse
  Endfor
 Endfor

  Omega_matrftLow=complexarr(pmax,nmax)
  Omega_matrftHigh=complexarr(pmax,nmax)
  Omega_matrftHigh2=complexarr(pmax,nmax)
  Omega_matrftHigh3=complexarr(pmax,nmax)
  Omega_matOrgft=complexarr(2*pmax-1,nmax,N_mu*(2*N_FT-1))
  Omega_matOrgft(*,*,*)=complex(0,0)
  openr,lun,'Omega_matrft.txt',/get_lun   ;;0 Gyro-kinetic
  readf,lun,Omega_matOrgft
  n=eof(lun)
  if n ne 1 then print,'error with file load!!!!!!'
  free_lun,lun
 For p=0,pmax-1 Do Begin
  For n=0,nmax-1 Do Begin
  tempRe=abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,0)))
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
      IF(tempRe LE abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j)))) THEN BEGIN
           tempRe= abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j)))
       Endif
    Endfor
    IF(N_FT GE 2)then begin
    tempRe=tempRe/Omega_star*tempReconst/(N_FT-1);!!!!!!!!!!!! 3. 4. !!used as the limiter of the low freq and high freq
    Endif

    jtemp=-1
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
          IF((abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))) LE tempRe))THEN BEGIN
          jtemp=j
          growthIm=(imaginary(Omega_matOrgft(p+pmax-1,n,j)))
          EndIf
    Endfor
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))) LE tempRe)THEN BEGIN
           IF(growthIm LT (imaginary(Omega_matOrgft(p+pmax-1,n,j)))) THEN BEGIN
           growthIm=imaginary(Omega_matOrgft(p+pmax-1,n,j))
           jtemp=j
           Endif
          Endif
    Endfor
    IF(jtemp EQ -1)then begin
    Omega_matrftLow(p,n)=complex(0,0)
    Endif else begin
    Omega_matrftLow(p,n)=Omega_matOrgft(p+pmax-1,n,jtemp)
    Endelse

    jtemp=-1
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))) GE tempRe) THEN BEGIN
            If(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))-REAL_PART(Omega_matrcyHigh(p,n))) LT freqPer*abs(REAL_PART(Omega_matrcyHigh(p,n)))) THEN BEGIN
             growthIm=imaginary(Omega_matOrgft(p+pmax-1,n,j))
             jtemp=j
            Endif
          EndIf
    Endfor
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))) GE tempRe) THEN BEGIN
            If(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))-REAL_PART(Omega_matrcyHigh(p,n))) LT freqPer*abs(REAL_PART(Omega_matrcyHigh(p,n)))) THEN BEGIN
             IF((imaginary(Omega_matOrgft(p+pmax-1,n,j))) GT growthIm) THEN BEGIN
             growthIm=imaginary(Omega_matOrgft(p+pmax-1,n,j))
             jtemp=j
             Endif
            Endif
          EndIf
    Endfor
    IF(jtemp EQ -1)then begin
    Omega_matrftHigh(p,n)=complex(0,0)
    Endif else begin
    Omega_matrftHigh(p,n)=Omega_matOrgft(p+pmax-1,n,jtemp)
    Endelse


    jtemp=-1
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))) GE tempRe) THEN BEGIN
            If(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))-2*REAL_PART(Omega_matrcyHigh(p,n))) LT freqPer*abs(REAL_PART(Omega_matrcyHigh(p,n)))) THEN BEGIN
             growthIm=imaginary(Omega_matOrgft(p+pmax-1,n,j))
             jtemp=j
            Endif
          EndIf
    Endfor
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))) GE tempRe) THEN BEGIN
            If(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))-2*REAL_PART(Omega_matrcyHigh(p,n))) LT freqPer*abs(REAL_PART(Omega_matrcyHigh(p,n)))) THEN BEGIN
             IF((imaginary(Omega_matOrgft(p+pmax-1,n,j))) GT growthIm) THEN BEGIN
             growthIm=imaginary(Omega_matOrgft(p+pmax-1,n,j))
             jtemp=j
             Endif
            Endif
          EndIf
    Endfor
    IF(jtemp EQ -1)then begin
    Omega_matrftHigh2(p,n)=complex(0,0)
    Endif else begin
    Omega_matrftHigh2(p,n)=Omega_matOrgft(p+pmax-1,n,jtemp)
    Endelse

    jtemp=-1
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))) GE tempRe) THEN BEGIN
            If(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))-3*REAL_PART(Omega_matrcyHigh(p,n))) LT freqPer*abs(REAL_PART(Omega_matrcyHigh(p,n)))) THEN BEGIN
             growthIm=imaginary(Omega_matOrgft(p+pmax-1,n,j))
             jtemp=j
            Endif
          EndIf
    Endfor
    For j=0,N_mu*(2*N_FT-1)-1 Do Begin
          IF(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))) GE tempRe) THEN BEGIN
            If(abs(REAL_PART(Omega_matOrgft(p+pmax-1,n,j))-3*REAL_PART(Omega_matrcyHigh(p,n))) LT freqPer*abs(REAL_PART(Omega_matrcyHigh(p,n)))) THEN BEGIN
             IF((imaginary(Omega_matOrgft(p+pmax-1,n,j))) GT growthIm) THEN BEGIN
             growthIm=imaginary(Omega_matOrgft(p+pmax-1,n,j))
             jtemp=j
             Endif
            Endif
          EndIf
    Endfor
    IF(jtemp EQ -1)then begin
    Omega_matrftHigh3(p,n)=complex(0,0)
    Endif else begin
    Omega_matrftHigh3(p,n)=Omega_matOrgft(p+pmax-1,n,jtemp)
    Endelse

  Endfor
 Endfor

   xbar=indgen(pbar)/float(pbar)
   ybar=indgen(pbar)/float(pbar)
   bars=dblarr(pbar,pbar)
;~~~color bar generation
  Omegacy=complex(0,0)
  Omegaft=complex(0,0)
  IF(!Fignum2 eq 1) Then Begin
   If(!Omegap eq 0)then begin
   zdh=REAL_PART(Omega_matrFinal)
   name[0]='Re('+cgGreek('omega')+') (GK)'
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zdh=imaginary(Omega_matrFinal)
   name[0]='Im('+cgGreek('omega')+') (GK)'
   Endif
  Endif Else IF(!Fignum2 eq 2) THEN BEGIN
   If(!Omegap eq 0)then begin
   zdh=REAL_PART(Omega_matrcyLow)
   name[0]='Re('+cgGreek('omega')+') (CKinCH) low freq'
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zdh=imaginary(Omega_matrcyLow)
   name[0]='Im('+cgGreek('omega')+') (CKinCH) low freq'
   Endif
  Endif Else IF(!Fignum2 eq 3) THEN BEGIN
   If(!Omegap eq 0)then begin
   zdh=REAL_PART(Omega_matrcyHigh)
   name[0]='Re('+cgGreek('omega')+') (CKinCH) high freq'
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zdh=imaginary(Omega_matrcyHigh)
   name[0]='Im('+cgGreek('omega')+') (CKinCH) high freq'
   Endif
  Endif Else IF(!Fignum2 eq 4) THEN BEGIN
   If(!Omegap eq 0)then begin
   zdh=REAL_PART(Omega_matrftLow)
   name[0]='Re('+cgGreek('omega')+') (CKinFH) low freq' 
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zdh=imaginary(Omega_matrftLow)
   name[0]='Im('+cgGreek('omega')+') (CKinFH) low freq'  
   Endif
  Endif Else IF(!Fignum2 eq 5) THEN BEGIN
   If(!Omegap eq 0)then begin
   zdh=REAL_PART(Omega_matrftHigh)
   name[0]='Re('+cgGreek('omega')+') (CKinFH) high freq'  
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zdh=imaginary(Omega_matrftHigh)
   name[0]='Im('+cgGreek('omega')+') (CKinFH) high freq' 
   Endif
  Endif Else IF(!Fignum2 eq 6) THEN BEGIN
   If(!Omegap eq 0)then begin
   zdh=REAL_PART(Omega_matrftHigh2)
   name[0]='Re('+cgGreek('omega')+') (CKinFH) high freq 2' 
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zdh=imaginary(Omega_matrftHigh2)
   name[0]='Im('+cgGreek('omega')+') (CKinFH) high freq 2'
   Endif
  Endif Else IF(!Fignum2 eq 7) THEN BEGIN
   If(!Omegap eq 0)then begin
   zdh=REAL_PART(Omega_matrftHigh3)
   name[0]='Re('+cgGreek('omega')+') (CKinFH) high freq 3'  
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zdh=imaginary(Omega_matrftHigh3)
   name[0]='Im('+cgGreek('omega')+') (CKinFH) high freq 3' 
   Endif
  Endif
   zmax=max(zdh)+(max(zdh)-min(zdh))/10
   zmin=min(zdh)-(max(zdh)-min(zdh))/10
   ;zdh=zdh+0.005
   If((zmax eq 0) and (zmin eq 0))then begin
   zmax=1
   zmin=-1
   endif
   vb1=zmax
   vb2=zmin
   for i=0,pbar-1 do begin
       for j=0,pbar-1 do begin
           bars(i,j)=j/double(pbar)*(vb1-vb2)+vb2
       endfor
   endfor
   ybar=ybar*(vb1-vb2)+vb2
   contour,bars,xbar,ybar,$
   title=' ',$
   xticks=2,$                ;;让x轴上不标字
   xtickname=[' ',' ',' '],$ ;;让x轴上不标字
   /fill,$
   nlevels=levs,$
   c_color=ccol,$
   position=posi1,$
   zstyle=1,xstyle=1,ystyle=1,$  ;;值1让图像充满坐标轴
   yrange=[zmin,zmax]

   contour,zdh(*,*),kxhalf,kyhalf,$
   title=name[0],xtitle='kx',ytitle='ky',$
   /fill,nlevels=levs,$
   c_color=ccol,$
   position=posi2,$
   zstyle=1,ystyle=1,$
   xstyle=1,$
   zrange=[zmin,zmax],$
   /noerase

   goto, Matrixtheend
   


;;----------------------------------------------------------------------   

Endif Else IF( strcmp(name[0],'|ne|',4) ) THEN BEGIN
;      zd=fltarr(2*pmax-1,nmax)
;      nsqr_k=fltarr(2*pmax-1,nmax,ntmax/output_step+1)
;      openr,lun,'nsqr_k.txt',/get_lun
;      readf,lun,nsqr_k
;      free_lun,lun
;      
;      zd=total(nsqr_k(*,*,round(sattime/(output_step*tstep)+1):round(ntmax/output_step)),3);the average energy of steady state
;      zd=zd/(ntmax/output_step-sattime/(output_step*tstep))
  
      zd=fltarr(2*pmax-1,nmax)
      
      nesat=fltarr(2*pmax-1,nmax)
      jump=1
      IF(ntmax/output_step GE 1000)then begin
      jump=10
      Endif Else IF(ntmax/output_step GE 10000)then begin
      jump=100
      Endif Else IF(ntmax/output_step GE 100000)then begin
      jump=1000
      Endif Else IF(ntmax/output_step GE 1000000)then begin
      jump=10000
      Endif

      nsqr_k=complexarr(2*pmax-1,2*nmax-1,ntmax/(output_step*jump)+1)
      nsqrtmp=complexarr(2*pmax-1,2*nmax-1)

      openr,lun,'ne_k.txt',/get_lun
      readf,lun,nsqrtmp
      POINT_LUN, -lun, pos
      Print,'Pos=',pos
      For i=0L,ntmax/output_step,jump Do begin
      POINT_LUN, lun, pos*i
      readf,lun,nsqrtmp
      nsqr_k(*,*,i/jump)=nsqrtmp
      Endfor
      free_lun,lun

For j=round(sattime/(output_step*jump*tstep)),round(ntmax/(output_step*jump)) Do Begin
;nesat(*,*)=nesat(*,*)+abs(nsqr_k(*,nmax-1:2*nmax-2,j))
nesat(*,*)=nesat(*,*)+sqrt(conj(nsqr_k(*,nmax-1:2*nmax-2,j))*nsqr_k(*,nmax-1:2*nmax-2,j))
Endfor
nesat(*,*)=nesat(*,*)*(1.0/(ntmax/(output_step*jump)-sattime/(output_step*jump*tstep)))
   
      zd=nesat
  
            
   xbar=indgen(pbar)/float(pbar)
   ybar=indgen(pbar)/float(pbar)
   bars=dblarr(pbar,pbar)
      
   zmax=max(zd)+(max(zd)-min(zd))/10
   zmin=min(zd)-(max(zd)-min(zd))/10
   If((zmax eq 0) and (zmin eq 0))then begin
   zmax=1
   zmin=-1
   endif
   vb1=zmax
   vb2=zmin
   for i=0,pbar-1 do begin
       for j=0,pbar-1 do begin
           bars(i,j)=j/double(pbar)*(vb1-vb2)+vb2
       endfor
   endfor
   
   ybarp=indgen(pbar)/float(pbar)
   ybarp=ybarp*(vb1-vb2)+vb2


   contour,bars,xbar,ybarp,$
   title=' ',$
   xticks=2,$                ;;让x轴上不标字
   xtickname=[' ',' ',' '],$ ;;让x轴上不标字
   /fill,$
   nlevels=levs,$
   c_color=ccol,$
   position=posi1,$
   zstyle=1,xstyle=1,ystyle=1,$  ;;值1让图像充满坐标轴
   yrange=[zmin,zmax]

   contour,zd(*,*),kx,kyhalf,$
   title=name[0],xtitle='kx',ytitle='ky',$
   /fill,nlevels=levs,$
   c_color=ccol,$
   position=posi2,$
   zstyle=1,ystyle=1,$
   xstyle=1,$
   zrange=[zmin,zmax],$
   /noerase


Endif

;;----------------------------------------------------------------------
IF( strcmp(name[0],'|Phi|^2',7) ) THEN BEGIN
  tempReconst=1  ;!!!!!!!!!!!!!!!!
  
  zdh=fltarr(2*pmax-1,2*nmax-1)

   xbar=indgen(pbar)/float(pbar)
   ybar=indgen(pbar)/float(pbar)
   bars=dblarr(pbar,pbar)
;~~~color bar generation
  
 ;;--------------------------------------------------------- 

;    Print,"input jump:"
;    read,jump
;    Print,"input imax_deno"
;    read,imax_deno
    
    jump=100
    imax_deno=1
    
    
   ibegin=0LL
   IF(Stopnt EQ 0)then begin
   Nsat=round(ntmax-begintime/tstep)/output_step     
   ibegin=round(begintime/tstep/output_step)
   ENDIF   
   IF(Stopnt GT 0)then begin
   Nsat=(ntmax-Stopnt)/output_step  
   ibegin=0  
   ENDIF
       

    imax=Nsat/imax_deno
    Time=findgen(imax/jump)*(jump*tstep*output_step) + Stopnt*tstep + begintime    
    Phi_ktmp=complexarr(2*pmax-1,2*nmax-1)  
    z=complexarr(2*pmax-1,2*nmax-1) 
    zphi=fltarr(2*pmax-1,2*nmax-1,imax/jump) 
    Phi_k=complexarr(2*pmax-1,2*nmax-1,imax/jump)  
    x=FINDGEN(2*pmax-1)
    y=FINDGEN(2*pmax-1)
    x=(x-pmax+1)/(kxmax/(pmax-1))/(2*pmax-2)*2*!PI
    y=(y-nmax+1)/(kymax/(nmax-1))/(2*nmax-2)*2*!PI
    
      pos=0LL
      openr,lun,'Phi_kOMG.txt',/get_lun
      readf,lun,Phi_ktmp
      POINT_LUN, -lun, pos
      Print,'Pos=',pos
      For i=0LL,imax-1,jump Do begin
      POINT_LUN, lun, pos*(i+ibegin) 
      readf,lun,Phi_ktmp
      Phi_k(*,*,i/jump)=Phi_ktmp
      Endfor
      free_lun,lun       
      
   For i=0LL,imax-1,jump Do Begin
   z=Phi_k(*,*,i/jump)
   z=shift(z,-pmax+1,-nmax+1)
   z = FFT(z,/INVERSE)
   If(log eq 0)then begin
   zphi(*,*,i/jump) = (REAL_PART(z))
   Endif Else Begin
     For p=0,2*pmax-2 Do Begin
     For n=0,2*nmax-2 Do Begin
     IF(REAL_PART(z(p,n)) GT 0)then begin
     zphi(*,*,i/jump) = Alog10(abs(REAL_PART(z(p,n))))
     ENDIF
     IF(REAL_PART(z(p,n)) LT 0)then begin
     zphi(*,*,i/jump) = -Alog10(abs(REAL_PART(z(p,n))))
     ENDIF
     Endfor 
     Endfor 
   Endelse
   
   ;zphi(*,*,i/jump)=zphi(*,*,i/jump)-total(total(zphi(*,*,i/jump),1),1)/(2*pmax-1)/(2*nmax-1)
   Endfor 
        
   print,'average=',total(total(total(zphi(*,*,*),1),1),1)/(2*pmax-1)/(2*nmax-1)/(imax/jump)        
   ;zphi=zphi-total(total(total(zphi(*,*,*),1),1),1)/(2*pmax-1)/(2*nmax-1)/(imax/jump)
   
    zdh=zphi(*,*,0)
        
;   top=max(zphi)
;   bottom=min(zphi)
;   zmax=min([abs(top),abs(bottom)])*1
;   zmin=-min([abs(top),abs(bottom)])*1
   
   zmax=20
   zmin=-20
   
   print,'zmax=',max(zphi)
   print,'zmin=',min(zphi)

   
   If((zmax eq 0) and (zmin eq 0))then begin
   zmax=1
   zmin=-1
   endif
   vb1=zmax
   vb2=zmin
   for i=0,pbar-1 do begin
       for j=0,pbar-1 do begin
           bars(i,j)=j/double(pbar)*(vb1-vb2)+vb2
       endfor
   endfor
   ybar=ybar*(vb1-vb2)+vb2
   contour,bars,xbar,ybar,$
   title='',$
   xticks=2,$                ;;让x轴上不标字
   xtickname=[' ',' ',' '],$ ;;让x轴上不标字
   /fill,$
   nlevels=levs,$
   c_color=ccol,$
   position=posi1,$
   zstyle=1,xstyle=1,ystyle=1,$  ;;值1让图像充满坐标轴
   yrange=[zmin,zmax]

   contour,zdh(*,*),x,y,$
   title='',$
   xtitle='x',ytitle='y',$
   /fill,nlevels=levs,$
   c_color=ccol,$
   position=posi2,$
   zstyle=1,ystyle=1,$
   xstyle=1,$
   /noerase,$
   zrange=[zmin,zmax]

 
 IF(!velocity EQ 0)then Begin
 ;------------------------------------------------     
  For i=0LL,imax-1,jump Do Begin  ;;the main loop of the movie
   zdh=zphi(*,*,i/jump)
   contour,zdh(*,*),x,y,$
   ;title='phi:  '+strtrim(Time(i/jump),2)+'a/cs',$
   xtitle='x',ytitle='y',$
   /fill,nlevels=levs,$
   c_color=ccol,$
   position=posi2,$
   zstyle=1,ystyle=1,$
   xstyle=1,$
   zrange=[zmin,zmax],$
   /noerase,$
   /OVERPLOT
   
   xyouts,-10,17,'phi:  '+strtrim(Time(i/jump),2)+'   a/cs'
   wait,0.05
  Endfor
 ;------------------------------------------------
  yline=total(total((zphi),1),1)/(2*pmax-1)/(2*nmax-1)
  WINDOW,2, XSIZE=700, YSIZE=700   
   plot,[0],[0],$
       /nodata,$
       xstyle=1,$
       ystyle=1,$
       yminor=0,$
       xrange=[min(Time),max(Time)],$
       yrange=[min(yline)*1,max(yline)*1.02],$       
       xtitle='Time',$
        title="Total "+cgGreek('phi')+' (CK)'$
        ,psym=-5 ,symsize=1.5;,/xlog;,thich=8.0
        
    oplot,Time,yline,thick=0.1    
 ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
 Endif else if(!velocity GT 0)then Begin
   print,'!velocity=',!velocity
   i=!velocity*jump
   zdh=zphi(*,*,i/jump)
   
   contour,zdh(*,*),x,y,$
   ;title='phi:  '+strtrim(Time(i/jump),2)+'a/cs',$
   xtitle='x',ytitle='y',$
   /fill,nlevels=levs,$
   c_color=ccol,$
   position=posi2,$
   zstyle=1,ystyle=1,$
   xstyle=1,$
   zrange=[zmin,zmax],$
   /noerase,$
   /OVERPLOT   
   xyouts,-10,23,'phi:  '+strtrim(Time(i/jump),2)+'   a/cs'
   
   deltax=1/(kxmax/(pmax-1))/(2*pmax-2)*2*!PI   
   deltay=1/(kymax/(nmax-1))/(2*nmax-2)*2*!PI
   const=1.8
      
      
   For p=0,2*pmax-3 Do begin
     For n=0,2*nmax-3 Do begin
     oplot,[x(p),x(p)+(zdh(p,n+1)-zdh(p,n))/deltay/const],$
     [y(n),y(n)+(-zdh(p+1,n)+zdh(p,n))/deltax/const],$
     thick=1.0
     
     !p.color=color_value(ncolor*8/16)  ;;white 
     oplot,[x(p)+(zdh(p,n+1)-zdh(p,n))/deltay/const,x(p)+(zdh(p,n+1)-zdh(p,n))/deltay/const],$
     [y(n)+(-zdh(p+1,n)+zdh(p,n))/deltax/const,y(n)+(-zdh(p+1,n)+zdh(p,n))/deltax/const],$
     thick=1,psym=4 ,symsize=0.2
     
      !p.color=color_value(ncolor+1)  ;black  
     Endfor
   Endfor

;--------------------------------------------
; plot velocity vector
   !P.BACKGROUND=color_value(ncolor/2)
    WINDOW,2, XSIZE=500, YSIZE=500  
   ; !P.COLOR=color_value(ncolor/2)
    
     v_eddy=fltarr(2*pmax-1,2*nmax-1)  
   For p=0,2*pmax-2 Do begin
     For n=0,2*nmax-2 Do begin
       if(n eq 2*nmax-2)then begin ;;cyclic boundary condition
       n_p=0
       endif else begin
       n_p=n+1
       endelse
       if(p eq 2*pmax-2)then begin ;;cyclic boundary condition
       p_p=0
       endif else begin
       p_p=p+1
       endelse   
            
       IF(zdh(p,n) GE 0)then begin    
       v_eddy(p,n)=sqrt(((zdh(p,n_p)-zdh(p,n))/deltay)^2+((-zdh(p_p,n)+zdh(p,n))/deltax)^2)
       Endif Else begin
       v_eddy(p,n)=-sqrt(((zdh(p,n_p)-zdh(p,n))/deltay)^2+((-zdh(p_p,n)+zdh(p,n))/deltax)^2)
       Endelse
     Endfor
   Endfor
   

   zmin=-max([abs(min(v_eddy)),abs(max(v_eddy))])
   zmax=max([abs(min(v_eddy)),abs(max(v_eddy))])
;   zmin=-10
;   zmax=10
   print,'vmax=',zmin
   print,'vmin=',zmax
   
   If((zmax eq 0) and (zmin eq 0))then begin
   zmax=1
   zmin=-1
   endif
   vb1=zmax
   vb2=zmin
   for i=0,pbar-1 do begin
       for j=0,pbar-1 do begin
           bars(i,j)=j/double(pbar)*(vb1-vb2)+vb2
       endfor
   endfor
   ybar=ybar*(vb1-vb2)+vb2
   contour,bars,xbar,ybar,$
   title='',$
   xticks=2,$                ;;让x轴上不标字
   xtickname=[' ',' ',' '],$ ;;让x轴上不标字
   /fill,$
   nlevels=levs,$
   c_color=ccol,$
   position=posi1,$
   zstyle=1,xstyle=1,ystyle=1,$  ;;值1让图像充满坐标轴
   yrange=[zmin,zmax]
         
     
   contour,v_eddy(*,*),x,y,$
   xtitle='x',ytitle='y',$
   /fill,nlevels=levs,$
   c_color=ccol,$
   position=posi2,$
   zstyle=1,ystyle=1,$
   xstyle=1,$
   zrange=[zmin,zmax],$
   /noerase;,$
   ;/OVERPLOT   
   
   i=!velocity*jump
   xyouts,-10,23,'|v| :  '+strtrim(Time(i/jump),2)+'   a/cs'
   
 Endif
   
Endif else begin
;;--------------------------------------------------------------------------------------

   if neps eq 0 then begin
   ;   window,3,xsize=700,ysize=700;,/pixmap
   endif else begin
      snapname=name[0]+string(rms,format='(I1)')+'bin'+'.eps'
      set_plot,'PS'  ;;Change the IDL graphics device to PostScript（为了 输出为eps图像）
      device,color=1,/encapsul,file=snapname,xsize=20,ysize=20,bits_per_pixel=8
      ;;provides device-dependent control over the current graphics device (as set by the SET_PLOT routine).
     ;;/encapsul :Set this keyword to create an encapsulated PostScript file, suitable for importing into another document (e.g., a LaTeX or FrameMaker document).
     ;;pixel像素的意思
   endelse

   xbar=indgen(pbar)/float(pbar)
   ybar=indgen(pbar)/float(pbar)
   bars=dblarr(pbar,pbar)

;~~~color bar generation
  IF(!Fignum eq 1) Then Begin
   If(!Omegap eq 0)then begin
   zd=REAL_PART(Omega)
   name[0]='Re('+cgGreek('omega')+') (GK) sim' 
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zd=imaginary(Omega)
   name[0]='Im('+cgGreek('omega')+') (GK) sim'
   Endif
  Endif Else IF(!Fignum eq 2) THEN BEGIN
   If(!Omegap eq 0)then begin
   zd=REAL_PART(Omegacy)
   name[0]='Re('+cgGreek('omega')+') (CKinCH) sim'  
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zd=imaginary(Omegacy)
   name[0]='Im('+cgGreek('omega')+') (CKinCH) sim'  
   Endif
  Endif Else IF(!Fignum eq 3) THEN BEGIN
   If(!Omegap eq 0)then begin
   zd=REAL_PART(Omegaft)
   name[0]='Re('+cgGreek('omega')+') (CKinFH) sim' 
   Endif Else IF(!Omegap eq 1) THEN BEGIN
   zd=imaginary(Omegaft)
   name[0]='Im('+cgGreek('omega')+') (CKinFH) sim'  
   Endif
  Endif
 CKinCHzmax=max(zd)+(max(zd)-min(zd))/10
   zmin=min(zd)-(max(zd)-min(zd))/10
   If((zmax eq 0) and (zmin eq 0))then begin
   zmax=1
   zmin=-1
   endif
   vb1=zmax
   vb2=zmin
   for i=0,pbar-1 do begin
       for j=0,pbar-1 do begin
           bars(i,j)=j/double(pbar)*(vb1-vb2)+vb2
       endfor
   endfor
   
   ybarp=indgen(pbar)/float(pbar)
   ybarp=ybarp*(vb1-vb2)+vb2


   contour,bars,xbar,ybarp,$
   title=' ',$
   xticks=2,$                ;;让x轴上不标字
   xtickname=[' ',' ',' '],$ ;;让x轴上不标字
   /fill,$
   nlevels=levs,$
   c_color=ccol,$
   position=posi1,$
   zstyle=1,xstyle=1,ystyle=1,$  ;;值1让图像充满坐标轴
   yrange=[zmin,zmax]

   contour,zd(*,*),kxhalf,kyhalf,$
   title=name[0],xtitle='kx',ytitle='ky',$
   /fill,nlevels=levs,$
   c_color=ccol,$
   position=posi2,$
   zstyle=1,ystyle=1,$
   xstyle=1,$
   zrange=[zmin,zmax],$
   /noerase

   Matrixtheend:

   if neps ne 0 then begin
      device,/close_file
      set_plot,!device_org
   End

help,loadct



Endelse
 return
end

;*******************************************************************************


pro turbulence3D_event,event,group_leader=group

  common startup,number_plot,fpath,ncolor,color_value,plotid

  widget_control,event.id,get_uvalue=choice

  case choice of

    0: begin            ;exit idl
      widget_control, event.top,/destroy
      exit
    end

    1: begin            ;hplt
      number_plot=number_plot+1
      Main_plots_name
    end

;    2: begin            
;      number_plot=number_plot+1
;      More_details_name
;    end
;    
;    3: begin            ;hplt
;      number_plot=number_plot+1
;      Colorful_plot_name
;    end
;
;    4: begin            ;movie for 2d data
;      number_plot=number_plot+1
;      Movie
;    end

    2: begin            ;colormap

      number_plot=number_plot+1
      window, number_plot, TITLE='snap', xsize=600,ysize=600
      !p.thick=16

      xmin=-float(ncolor+2)/2
      xmax=float(ncolor+2)/2
      ymin=0.0
      ymax=1.0
      set_xy,xmin,xmax,ymin,ymax

      for i=0,ncolor+1 do begin
        xa=xmin+float(i)
        x=[xa,xa]
        y=[0.0,1.0]
        !p.color=color_value(i)
        oplot,x,y
      endfor
      !p.thick=1

    end

    3: begin            ;exit idl
      widget_control, event.top,/destroy
      exit
    end

  endcase
end

    ; plot program for 3D turbulence code  ;;!!Main process
common startup,number_plot,fpath,ncolor,color_value,plotid


  defsysv,'!vuM6DFolder',"a"
  defsysv,'!workFolder',"a"

  cd, current=vuM6DFolder   ; Note that it can not use:  cd, current=!vuM6DFolder
  !vuM6DFolder=vuM6DFolder

  workFolder = DIALOG_PICKFILE( /DIRECTORY, $
  TITLE="Choose working directory.")
   
  !workFolder=workFolder
   
  print,'!vuM6DFolder  ',!vuM6DFolder
  print,'!workFolder  ',!workFolder

  cd, !vuM6DFolder


; read color map
 openr,1,'color.dat'
 ncolor=1
 readf,1,ncolor
 red=intarr(ncolor+2)
 green=intarr(ncolor+2)
 blue=intarr(ncolor+2)
 readf,1,red,green,blue
 close,1

 color_value=indgen(ncolor+2)

nbyte=3
; print, 'input 1 for 8-bit color display, 3 for true color''
; read, nbyte

;true color
 if nbyte eq 3 then color_value=red+256l*(green+256l*blue)

; load 8-bits color table
 if nbyte eq 1 then tvlct,red,green,blue

; default setting
; set_plot,'X'    ;================= We must remove this line on my computer
 defsysv,'!device_org',!D.NAME
 c=1
 !linetype=0
 !p.thick=2
 !p.charsize=2
 !p.charthick=2
 


Number_plot=0
fpath='.'

plotid=Number_plot

;pname=strarr(5)
;pname=["Exit IDL","Main plots","More details","Colorful Plot","Movie","ColorMap"]

pname=strarr(3)
pname=["Exit IDL","Main plots","ColorMap"]

xmenu,pname,BASE=pbase,SPACE=10,TITLE='3D turbulence',xpad=20,ypad=20
widget_control,pbase,/realize
xmanager,"turbulence3D",pbase,group_leader=group,/NO_BLOCK

end


