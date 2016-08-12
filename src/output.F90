module output

  use beale
  use types

contains

  subroutine write_R_script(pConfig,pBestConfig,pFlow,pConc)

    type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

    type (CONFIG_T), pointer :: pBestConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

    type (FLOW_T), dimension(:), pointer :: pFlow
    type (CONC_T), dimension(:), pointer :: pConc

    character(len=256) :: s_WD = ""
    character(len=256) :: s_RD = ""
    character(len=1) :: sQt = CHAR(34)   ! ASCII for "
    character(len=1) :: sPct = CHAR(37)  ! ASCII for % sign
    character(len=8) :: sPM = CHAR(34)//" %+-% "//CHAR(34)
    character(len=256) :: sUnits, sSite, sConstituent,sOutputFilePrefix
    character(len=256) :: sR_ScriptName, sPDF_PNG_Prefix
    character(len=256) :: sConstituentAllCaps, sAxisLabel, sSubTitle
    integer (kind=T_INT) :: i, j, iCount,iPos, iStat
    integer (kind=T_INT) :: iBMonth,iBDay,iBYear
    integer (kind=T_INT) :: iEMonth,iEDay,iEYear
    integer (kind=T_INT) :: iMonth,iDay,iYear
    real (kind=T_REAL) :: rOffset,rMaxConc

    iBMonth = pFlow(1)%iMonth
    iBDay = pFlow(1)%iDay
    iBYear = pFlow(1)%iYear

    iEMonth = pFlow(size(pFlow%rFlow))%iMonth
    iEDay = pFlow(size(pFlow%rFlow))%iDay
    iEYear = pFlow(size(pFlow%rFlow))%iYear

    iCount = 0
    iPos = 0

    rMaxConc = MAXVAL(pConc%rConc)*1.1 &
       / CONC_UNITS(pConfig%iConcUnitsCode)%rConversionFactor

    rOffset = 0.03 * rMaxConc

    sSite=pConc(1)%sTribName
    sConstituent = pConc(1)%sConstituentName
    sConstituentAllCaps = sConstituent
    call Uppercase(sConstituentAllCaps)

    if(pBestConfig%lJackknife) then

    write(sSubTitle,FMT="('Load: ',3a,5x,'RMSE: ',a," &
      //"5x,'Annual Load: ',3a,5x,'Annual Load (jackknife): ',3a)") &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedLoad))), &
      sPm, &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedLoadCI))), &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedRMSE))), &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedLoadAnnualized))), &
      sPm, &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedLoadAnnualizedCI))), &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rJackCombinedLoadAnnualized))), &
      sPm, &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rJackCombinedLoadAnnualizedCI)))

    else

    write(sSubTitle,FMT="('Load: ',3a,5x,'RMSE: ',a," &
      //"5x,'Annual Load: ',3a)") &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedLoad))), &
      sPm, &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedLoadCI))), &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedRMSE))), &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedLoadAnnualized))), &
      sPm, &
      trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rCombinedLoadAnnualizedCI)))

    endif

    sUnits = CONC_UNITS(pConfig%iConcUnitsCode)%sUnitsSpelledOut

    sAxisLabel = trim(sConstituentAllCaps)//', IN '//trim(sUnits)

    write(sOutputFilePrefix,FMT="(a,'_',a,'_',i4.4,i2.2,i2.2,'-'," &
        //"i4.4,i2.2,i2.2)") &
      trim(sSite),trim(sConstituent),iBYear,iBMonth,iBDay, &
      iEYear,iEMonth,iEDay

    call CleanUp(sOutputFilePrefix)

    do i=1,len_trim(pConfig%sBaseDirName)
      if(pConfig%sBaseDirName(i:i)=="\") then
        s_WD(i:i) = "/"
      else
        s_WD(i:i) = pConfig%sBaseDirName(i:i)
      end if
    end do

    do i=1,len_trim(pConfig%sResultsDirName)
      if(pConfig%sResultsDirName(i:i)=="\") then
        s_RD(i:i) = "/"
      else
        s_RD(i:i) = pConfig%sResultsDirName(i:i)
      end if
    end do

    sR_ScriptName = trim(pConfig%sBaseDirName)//trim(pConfig%sResultsDirName) &
          //trim(sOutputFilePrefix)//".R"

    sPDF_PNG_Prefix = trim(s_WD)//trim(s_RD) &
        //trim(sOutputFilePrefix)

    open (LU_R_SCRIPT, &
       file=sR_ScriptName, &
       STATUS='REPLACE',FORM='FORMATTED',iostat=iStat)

    if(iStat /= 0) then
      print *, "Could not open R Script output file: ", &
        trim(sOutputFilePrefix)//".R"
      stop
    end if


    write(LU_R_SCRIPT,FMT=*) &
      'flow<-data.frame(cbind(Date=1,Q=rep(0,',size(pFlow%rFlow),')))'
    write(LU_R_SCRIPT,FMT=*) &
      'colnames(flow)<-c("Date","Q")'

    write(LU_R_SCRIPT,FMT=*) &
      'conc<-data.frame(cbind(Date=1,Q=rep(0,',size(pConc%rConc),')))'
    write(LU_R_SCRIPT,FMT=*) &
      'colnames(conc)<-c("Date","Conc")'

    write(LU_R_SCRIPT,*) &
      'num_strata<-'//trim(int2char(pBestConfig%iMaxNumStrata))

    write(LU_R_SCRIPT,FMT=*) &
      'bound<-data.frame(Date=rep(0,',pBestConfig%iMaxNumStrata-1,'))'
    write(LU_R_SCRIPT,FMT=*) &
      'strata<-data.frame(Date=rep(0,',pBestConfig%iMaxNumStrata,'))'

    do i=1,size(pFlow%rFlow)
      write(LU_R_SCRIPT,&
         FMT="('flow$Date[', a, '] <- ', a1, i4.4, '-', i2.2, '-', i2.2, a1, '; flow$Q[', a, '] <-', a)") &
        trim(int2char(i)),sQt,pFlow(i)%iYear,pFlow(i)%iMonth,pFlow(i)%iDay, &
        sQt,trim(int2char(i)),trim(real2char(pFlow(i)%rFlow &
              / FLOW_UNITS(pConfig%iFlowUnitsCode)%rConversionFactor))
    end do

    do i=1,size(pConc%rConc)
      write(LU_R_SCRIPT,&
         FMT="('conc$Date[', a, '] <- ', a1, i4.4, '-', i2.2, '-', i2.2, a1 , '; conc$Conc[', a, '] <- ', a)") &
        trim(int2char(i)),sQt,pConc(i)%iYear,pConc(i)%iMonth,pConc(i)%iDay, &
        sQt,trim(int2char(i)),trim(real2char(pConc(i)%rConc &
              / CONC_UNITS(pConfig%iConcUnitsCode)%rConversionFactor))
    end do

    do i=1,pBestConfig%iMaxNumStrata - 1

      call gregorian_date(pBestConfig%iStrataBound(i), iYear, iMonth, iDay)

      write(LU_R_SCRIPT,&
         FMT="('bound$Date[',a,']<-',a1,i4.4,'-',i2.2,'-',i2.2,a1)") &
        trim(int2char(i)),sQt,iYear,iMonth,iDay,sQt

    end do


    do i=1,2

      if ( len_trim( s_WD ) > 0 )   write(LU_R_SCRIPT,FMT=*) 'setwd("'//trim(s_WD)//'")'
  !  write(LU_R_SCRIPT,FMT=*) &
  !    'y<-read.table("conc.txt",header=TRUE)'
      write(LU_R_SCRIPT,FMT=*) &
        'dummy<-runif(length(flow$Date))*max(conc$Conc)*1.1'
  !  write(LU_R_SCRIPT,FMT=*) &
  !    'conc$Date<-as.Date(paste(conc$Year,conc$Month,conc$Day,sep="-"))'
      write(LU_R_SCRIPT,FMT=*) &
        'flow$Date<-as.Date(flow$Date)'
      write(LU_R_SCRIPT,FMT=*) &
        'conc$Date<-as.Date(conc$Date)'

      if(pBestConfig%iMaxNumStrata>1) then
        write(LU_R_SCRIPT,FMT=*) &
          'bound$Date<-as.Date(bound$Date)'
      end if

      write(LU_R_SCRIPT,FMT=*) &
        'first<-flow$Date[1]'
      write(LU_R_SCRIPT,FMT=*) &
        'last<-flow$Date[length(flow$Date)]'
      write(LU_R_SCRIPT,FMT=*) &
        'datetext<-paste(format(first,format="%B %Y"),"through",format(last,format="%B %Y"))'

      write(LU_R_SCRIPT,*) &
        "strata$Date[1]<-mean(flow$Date[flow$Date<=bound$Date[1]])"

      write(LU_R_SCRIPT,*) &
        "strata$Date[num_strata]<-mean(flow$Date[flow$Date>bound$Date[num_strata-1]])"

      do j=2,pBestConfig%iMaxNumStrata-1
        write(LU_R_SCRIPT, &
          FMT="('strata$Date[',a,']<-mean(flow$Date[flow$Date>bound$Date['," &
          //"a,'] & flow$Date<=bound$Date[',a,']])')") &
          trim(int2char(j)),trim(int2char(j-1)),trim(int2char(j))
      end do

      if(i==1) then
        write(LU_R_SCRIPT,FMT=*) &
        'png(filename = '//sQt//trim(sPDF_PNG_Prefix)//'.png' &
        //sQt//', width = 1024, height = 768, units = '//sQt//'px'//sQt &
        //', pointsize = 12, bg = "white", res = NA)'     !, restoreConsole = TRUE)'
      else
        write(LU_R_SCRIPT,FMT=*) &
        'pdf(file = '//sQt//trim(sPDF_PNG_Prefix)//'.pdf' &
        //sQt//', width = 11, height = 8.5)'
      end if


      write(LU_R_SCRIPT,FMT=*) &
        'nf <- layout(matrix(c(1,2),2,1,byrow=F), width=2,height=c(5,1))'
      write(LU_R_SCRIPT,FMT=*) &
        '# set margins: bottom, left side, top, right side'
      write(LU_R_SCRIPT,FMT=*) &
        'par(mar=c(3,6,5,6))'

      write(LU_R_SCRIPT,FMT=*) &
        'par(bg="white")'
      write(LU_R_SCRIPT,FMT=*) &
        'par(tcl=0.35)'

      write(LU_R_SCRIPT,FMT=*) &
        'y<-flow$Q'
      write(LU_R_SCRIPT,FMT=*) &
        'y[y>median(y)]<-y[y>median(y)]*1.1'
      write(LU_R_SCRIPT,FMT=*) &
        'plot(x=flow$Date,y=y, type="n", axes=FALSE, ann=FALSE)'
      write(LU_R_SCRIPT,FMT=*) &
        'mtext("DISCHARGE, IN CFS",side=2,line=4,font=2,cex=1.1)'
  !    write(LU_R_SCRIPT,FMT=*) &
  !      'usr <- par("usr")'
  !    write(LU_R_SCRIPT,FMT=*) &
  !      'rect(usr[1], usr[3], usr[2], usr[4], col="cornsilk", border="black")'
      write(LU_R_SCRIPT,FMT=*) &
        'lines(x=flow$Date,y=flow$Q, col="steelblue")'
      write(LU_R_SCRIPT,FMT=*) &
        'points(x=flow$Date,y=flow$Q, pch=19, col="steelblue", cex=0.4)'
      write(LU_R_SCRIPT,FMT=*) &
        'axis(2, col.axis="blue", las=1)'
      write(LU_R_SCRIPT,FMT=*) &
        'axis.Date(1,flow$Date,col.axis='//sQt//'blue'//sQt//',format=' &
        //sQt//sPct//'b-'//sPct//'y'//sQt//',at=seq(as.Date(flow$Date[1]),' &
        //'as.Date(flow$Date[length(flow$Date)]),'//sQt//'months'//sQt//'))'

      write(LU_R_SCRIPT,FMT=*) &
        'axis.Date(1,flow$Date,side=3,' &
        //'labels=F,at=seq(as.Date(flow$Date[1]),' &
        //'as.Date(flow$Date[length(flow$Date)]),'//sQt//'months'//sQt//'))'



      write(LU_R_SCRIPT,FMT=*) &
        'title.txt<-paste('//sQt//trim(sSite)//': '//trim(sConstituent) &
        //' ('//sQt//',datetext,'//sQt//')'//sQt//')'

      write(LU_R_SCRIPT,FMT=*) &
        'title(main=title.txt, font.main=2, col.main='//sQt//'red'//sQt//',line=3)'
      write(LU_R_SCRIPT,FMT=*) &
        'mtext(expression('//sQt//trim(sSubTitle)//sQt//'),side=3,line=1,font=2,cex=0.9)'
  !    write(LU_R_SCRIPT,FMT=*) &
  !      'title(xlab=datetext, col.lab="red")'
      write(LU_R_SCRIPT,FMT=*) &
        'par(new=TRUE)'
      write(LU_R_SCRIPT,FMT=*) &
        'plot(x=flow$Date,y=dummy, type="n", axes=FALSE, ann=FALSE)'
      write(LU_R_SCRIPT,FMT=*) &
        'axis(4, col.axis="blue", las=1)'
      write(LU_R_SCRIPT,FMT=*) &
        'mtext('//sQt//trim(sAxisLabel)//sQt//',side=4,line=4,font=2,cex=1.1)'
      write(LU_R_SCRIPT,FMT=*) &
        'abline(v=conc$Date,col="grey80")'
      write(LU_R_SCRIPT,FMT=*) &
        'points(x=conc$Date,y=conc$Conc, pch=22, bg="red", cex=1.2)'

      write(LU_R_SCRIPT,FMT=*) &
        "if(mean(conc$Conc)<0.1) {"
      write(LU_R_SCRIPT,FMT= &
        "('text(x=conc$Date+2,y=conc$Conc + ',a,', sprintf('," &
        //"a,a,'.3f',a,',conc$Conc),cex=0.6,srt=90)')") &
          trim(real2char(rOffset)),sQt,sPct,sQt
        write(LU_R_SCRIPT,FMT=*) &
        "} else {"
      write(LU_R_SCRIPT,FMT= &
        "('text(x=conc$Date+2,y=conc$Conc + ',a,', sprintf('," &
        //"a,a,'.2f',a,',conc$Conc),cex=0.6,srt=90)')") &
          trim(real2char(rOffset)),sQt,sPct,sQt
      write(LU_R_SCRIPT,FMT=*) &
        "}"

  !    write(LU_R_SCRIPT,FMT=*) &
  !      'rect(as.Date("2000-05-01"),2,y$Date[18],6)'
      write(LU_R_SCRIPT,FMT=*) &
        'abline(v=bound$Date+1.5,col="red",lty=3,lwd=2)'

      do j=1,pBestConfig%iMaxNumStrata

        write(LU_R_SCRIPT,FMT= *) &
          'text(x=strata$Date['//trim(int2char(j))//'],y=0.97*max(dummy),' &
          //sQt//trim(ADJUSTL(sf_L_units(pConfig,pBestConfig%rStratumCorrectedLoad(j)))) &
          //sQt//',srt=90,cex=0.7,col='//sQt//'red'//sQt//')'

      end do

      write(LU_R_SCRIPT,FMT=*) &
        'box()'

      write(LU_R_SCRIPT,FMT=*) &
        'leg.txt<-c('//sQT//'DISCHARGE' &
          //sQt//','//sQt//'CONCENTRATION'//sQt//','//sQt &
          //'STRATA BOUNDARY'//sQt//')'
      write(LU_R_SCRIPT,FMT=*) &
        'leg.col<-c('//sQT//'steelblue'//sQt &
          //','//sQt//'black'//sQt//','//sQt//'red'//sQt//')'
      write(LU_R_SCRIPT,FMT=*) &
        'leg.cex<-0.8'
      write(LU_R_SCRIPT,FMT=*) &
        'leg.lty<-c(1,NA,3)'
      write(LU_R_SCRIPT,FMT=*) &
        'leg.lwd<-c(1,NA,2)'
      write(LU_R_SCRIPT,FMT=*) &
        'leg.pch<-c(19,22,NA)'
      write(LU_R_SCRIPT,FMT=*) &
        'pt.bg<-c('//sQT//'black'//sQT//',' &
          //sQT//'red'//sQT//','//sQT//'black'//sQT//')'
      write(LU_R_SCRIPT,FMT=*) &
        'par(mar=c(3,0,0,0))'
      write(LU_R_SCRIPT,FMT=*) &
        'plot(1:10,1:10,type='//sQT//'n'//sQT//',axes=F,ann=F)'

      write(LU_R_SCRIPT,FMT=*) &
        'mtext('//sQt//'Flow filename: ' &
        //trim(pConfig%sFlowFileName) &
        //'      Concentration filename: ' &
        //trim(pConfig%sConcFileName) &
        //sQt//',side=1,line=1,font=1,cex=0.7)'

      write(LU_R_SCRIPT,FMT=*) &
        'legend('//sQT//'center'//sQT//',legend=leg.txt,lty=leg.lty,' &
        //'col=leg.col,pch=leg.pch,lwd=leg.lwd,' &
        //'cex=leg.cex,pt.bg=pt.bg,inset=c(0.02,0.02),'&
        //'title='//sQT//'EXPLANATION'//sQT//')'


      write(LU_R_SCRIPT,FMT=*) 'dev.off()'

    end do

    close(LU_R_SCRIPT)

    call SYSTEM("Rscript "//trim(sR_ScriptName))


    return

  end subroutine write_R_script


  subroutine print_short_report(pConfig, pConc)

    type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                          ! program options, flags, and other settings
    type (CONC_T), dimension(:), pointer :: pConc

    character (len=1)   :: sTab = CHAR(9)
    character (len=32)  :: sTimeStamp

  !  if(pConfig%lJackknife) then

      sTimeStamp = make_timestamp()

      write(LU_SHORT_RPT, &
        FMT="(12a,i5,a,f18.2,a,a,a,5(f18.2,a),ES16.4,a,3(f18.2,a),i10)")  &
           trim(sTimeStamp), sTab,                                              &
          trim(pConfig%sFlowFileName),sTab,trim(pConfig%sConcFileName),sTab,    &
          trim(pConc(1)%sConstituentName), sTab,                                &
          trim(pConfig%sStartDate), sTab, trim(pConfig%sEndDate), sTab,         &
          pConfig%iMaxNumStrata,sTab,pConfig%rCombinedLoad                      &
            / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor,sTab,        &
          trim(LOAD_UNITS(pConfig%iLoadUnitsCode)%sUnits),sTab,                 &
          pConfig%rCombinedLoadCI                                               &
            / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
          pConfig%rCombinedLoadAnnualized                                       &
            / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
          pConfig%rCombinedLoadAnnualizedCI                                     &
            / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
          pConfig%rJackCombinedLoadAnnualized                                   &
            / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
          pConfig%rJackCombinedLoadAnnualizedCI                                 &
            / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
          pConfig%rTotalFlowAnnualized, sTab,                                   &
          pConfig%rCombinedMSE, sTab,pConfig%rCombinedRMSE,                     &
          sTab,pConfig%rCombinedEffectiveDegreesFreedom, sTab,                  &
          pConfig%iFuncCallNum

  !  else

  !    write(LU_SHORT_RPT, &
  !      FMT="(10a,i4,a,f18.2,a,a,a,3(f18.2,a),ES16.4,a,3(f18.2,a),i10)") &
  !        trim(pConfig%sFlowFileName),sTab,trim(pConfig%sConcFileName),sTab,&
  !        trim(pConc(1)%sConstituentName), sTab, &
  !        trim(pConfig%sStartDate), sTab, trim(pConfig%sEndDate), sTab, &
  !        pConfig%iMaxNumStrata,sTab,pConfig%rCombinedLoad,sTab, &
  !        trim(LOAD_UNITS(pConfig%iLoadUnitsCode)%sUnits),sTab, &
  !        pConfig%rCombinedLoadCI, sTab, &
  !        pConfig%rCombinedLoadAnnualized, sTab, &
  !        pConfig%rCombinedLoadAnnualizedCI, sTab, &
  !        pConfig%rTotalFlowAnnualized, sTab, &
  !        pConfig%rCombinedMSE, sTab,pConfig%rCombinedRMSE,&
  !        sTab,pConfig%rCombinedEffectiveDegreesFreedom, sTab, &
  !        pConfig%iFuncCallNum
  !
  !  end if

    flush(LU_SHORT_RPT)

    return

  end subroutine print_short_report

!--------------------------------------------------------------------------------------------------

  subroutine print_strata_summary(pConfig, iLU)

    !! [ ARGUMENTS ]
    type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                          ! program options, flags, and other settings

    integer (kind=T_INT), intent(in) :: iLU
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iSYear, iSMonth, iSDay
    integer (kind=T_INT) :: iEYear, iEMonth, iEDay


  !  write(iLU,FMT=*) repeat("-",80)

    write(iLU,FMT=*) ' SUMMARY OVER ALL STRATA:'

    write(iLU,FMT="('    NUMBER OF STRATA:',i4)") pConfig%iMaxNumStrata

    do i=1,pConfig%iMaxNumStrata

      call gregorian_date(pConfig%iStrataBound(i - 1) + 1, iSYear, iSMonth, iSDay)
      call gregorian_date(pConfig%iStrataBound(i), iEYear, iEMonth, iEDay)

      write(iLU,&
       FMT="(t7,i3,t15,' -- begins on:',3x,i2.2,'/',i2.2,'/'," &
         //"i4,3x'ends on:',3x,i2.2,'/',i2.2,'/',i4)") i,&
         iSMonth, iSDay, iSYear,iEMonth, iEDay, iEYear

    end do

    write(iLU, FMT="(/)")

    write(iLU,&
      FMT="('    COMBINED LOAD:',t31,a25)") &
        trim(sf_L_units(pConfig,pConfig%rCombinedLoad))
    write(iLU,&
      FMT="('    CONFIDENCE INTERVAL:',t31,a25)") &
        trim(sf_L_units(pConfig,pConfig%rCombinedLoadCI))

    write(iLU,FMT=*) " "

    write(iLU,&
      FMT="('    COMBINED LOAD (PER YEAR):',t31,a25,a3)") &
        trim(sf_L_units(pConfig,pConfig%rCombinedLoadAnnualized)),'/yr'

    write(iLU,&
      FMT="('    COMBINED LOAD CI:',t31,a25,a3)") &
        trim(sf_L_units(pConfig,pConfig%rCombinedLoadAnnualizedCI)),'/yr'

    write(iLU,&
      FMT="('    TOTAL ANNNUALIZED FLOW:',t31,ES16.4,a)") &
        pConfig%rTotalFlowAnnualized,' cubic meters'

    write(iLU,FMT=*) " "

    write(iLU,&
      FMT="('    COMBINED MSE:',t31,a28)") &
        trim(sf_L2_units(pConfig,pConfig%rCombinedMSE))
    write(iLU,&
      FMT="('    COMBINED RMSE:',t31,a25)") &
        trim(sf_L_units(pConfig,pConfig%rCombinedRMSE))

    write(iLU,&
      FMT="('    COMBINED DEGREES FREEDOM:',t31,13x,f12.2)") &
        pConfig%rCombinedEffectiveDegreesFreedom
    write(iLU,&
      FMT="('    NUMBER of strata boundary sets evaluated:',i10)") &
        pConfig%iFuncCallNum

    write(iLU,FMT=*) repeat("~",80)
    write(iLU,FMT="(' ===> END OF SUMMARY for calculation with ',i3,' strata')") &
      pConfig%iMaxNumStrata
    write(iLU,FMT=*) repeat("~",80)

    write(iLU,FMT=*) " "

    flush(iLU)

    return

  end subroutine print_strata_summary

end module output
