        program art
        double precision ope,maxd,mind,clo1,clo12,proDis
        double precision pc,sum1,EPoRe,TaxaFixa,periodo
        integer DiasAtivo,Ativos,indo,NA1,contDist,pDistri
        double precision passodi
        integer esco,ctes,kbt,tbk,tem


        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: retornoD,MedRet
        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: WVec,TWVec,MCor
        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: Desvio,sumvola
        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: vola,ISharpe
        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: Distri
        integer,dimension(:,:),allocatable:: jafoi

        double precision grnd

!       semente random number
        call sgrnd(1321554)!31167285

!       Carregando arquivos do histórico de preços

        OPEN(14, FILE="ITUB3LAST.txt", status="old")
        OPEN(15, FILE="BBAS3LAST.txt", status="old")
        OPEN(16, FILE="PETR4LAST.txt", status="old")
        OPEN(17, FILE="VALE3LAST.txt", status="old")
        OPEN(18, FILE="MGLU3LAST.txt", status="old")
        OPEN(19, FILE="SUZB3LAST.txt", status="old")

        open(11,file='dateF.dat')
        open(12,file='retornoD.dat')
        open(13,file='Portifolios.dat')
        open(10,file='distribuicao.dat')

!       parâmetros de entrada
        NA1 = 10000
        DiasAtivo = 1512
        Ativos = 6
        passodi = 0.0025
        TaxaFixa = 0.1075 !Selic
        periodo = 252.0

        pDistri = (0.15-(-0.15))/passodi


        ALLOCATE(retornoD(DiasAtivo-1,Ativos),WVec(1,Ativos))
        ALLOCATE(MedRet(1,Ativos),TWVec(Ativos,1))
        ALLOCATE(Distri(pDistri,Ativos+1))
        ALLOCATE(MCor(Ativos,Ativos),Desvio(1,Ativos))
        ALLOCATE(sumvola(1,1),vola(1,1),ISharpe(1,1))
        ALLOCATE(jafoi(1,Ativos))





        indo = 0

!       Calculando os retornos esperados
        do j = 14,13+Ativos

        indo = indo + 1
        read(j,*)ope,maxd,mind,clo1

        do i = 1,DiasAtivo-1

        read(j,*)ope,maxd,mind,clo12
        retornoD(i,indo) = log(clo12/clo1)

        clo1 = clo12
        enddo
        MedRet(1,indo) = sum(retornoD(:,indo))/(DiasAtivo-1)
        enddo


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!       Distribuição dos retornos
        Distri = 0.0
        do l = 1,Ativos
        proDis = -0.15-passodi

        do j = 1,pDistri+1
        proDis = proDis + passodi
        if(l .eq. 1)then
        Distri(j,1) = proDis
        end if
        contDist = 0

        do i = 1,DiasAtivo-1

        if((retornoD(i,l) .ge. proDis) .and. &
           (retornoD(i,l) .lt. proDis+passodi) )then
           contDist = contDist + 1
        end if

        end do

        Distri(j,l+1) = contDist

        enddo

        enddo


!       Escrevendo retorno e distribuição em arquivo
        do i = 1,DiasAtivo-1
        write(12,*)retornoD(i,1),retornoD(i,2), &
                   retornoD(i,3),retornoD(i,4), &
                   retornoD(i,5),retornoD(i,6)
        enddo

        do i = 1,pDistri
        write(10,*)Distri(i,1),Distri(i,2), &
                   Distri(i,3),Distri(i,4), &
                   Distri(i,5),Distri(i,6), &
                   Distri(i,7)
        enddo






!       DESVIO PADR¶O
        do j = 1,Ativos
        sum1 = 0.0
        do i = 1,(DiasAtivo-1)

        sum1 = sum1 + (retornoD(i,j)-MedRet(1,j))**2

        enddo
        Desvio(1,j) = sqrt(periodo*(sum1/(DiasAtivo-1)))
        enddo



!       CALCULANDO matriz de covariancia = variancia correlacionada:      c_x_y = [SUM_i{(x_i-<x>)*(y_i-<y>)}]/N
        write(*,*)"============================"
        write(*,*)"|| Matriz de Covariancia  ||"
        write(*,*)"============================"
        write(*,*)"||"
        do l = 1,Ativos
        do k = 1,Ativos

        sum1 = 0.0
        do m = 1,(DiasAtivo-1)
        sum1 = sum1 + ( (retornoD(m,l)-MedRet(1,l))* &
                        (retornoD(m,k)-MedRet(1,k))  )
        enddo
        MCor(l,k) = (sum1/(DiasAtivo-1))*periodo

        enddo

        if(l .eq. Ativos)then
        write(*,*)"|| ",MCor(l,:)," ||"
        write(*,*)"||"
        write(*,*)"============================"
        else
        write(*,*)"|| ",MCor(l,:)," ||"
        endif

        enddo

        write(*,*)"============================"
        write(*,*)"||   Matriz de Codesvio   ||"
        write(*,*)"============================"
        write(*,*)"||"
        do l = 1,Ativos
        write(*,*)"|| ",sqrt(MCor(l,:))," ||"
        enddo
        write(*,*)"||"
        write(*,*)"============================"


!       MC: Escolhendo aleatóriamente os portifólios
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       Porcentagens Iguais

        vola = 0.0
        WVec = 0.0
        ISharpe = 0.0
        sumvola = 0.0

        do i = 1,Ativos
        WVec(1,i) = 1.0/Ativos
        enddo
        EPoRe = 0.0
        do i = 1,Ativos
        EPoRe = EPoRe + MedRet(1,i)*periodo*WVec(1,i)
        enddo

        do i = 1,Ativos
        do j = 1,Ativos
        sumvola = sumvola + MCor(i,j)*WVec(1,i)*WVec(1,j)
        end do
        end do

        vola = sqrt(sumvola)
        ISharpe = (EPoRe - TaxaFixa)/vola
        write(13,*)WVec,sum(WVec),EPoRe,vola,ISharpe

!**************************************************************
!       Carteiras de 1 ativo
        do l = 1,Ativos

        vola = 0.0
        WVec = 0.0
        ISharpe = 0.0
        sumvola = 0.0

        WVec(1,l) = 1.0

        EPoRe = 0.0
        do i = 1,Ativos
        EPoRe = EPoRe + MedRet(1,i)*periodo*WVec(1,i)
        enddo

        do i = 1,Ativos
        do j = 1,Ativos
        sumvola = sumvola + MCor(i,j)*WVec(1,i)*WVec(1,j)
        end do
        end do

        vola = sqrt(sumvola)
        ISharpe = (EPoRe - TaxaFixa)/vola
        write(13,*)WVec,sum(WVec),EPoRe,vola,ISharpe

        end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       Aleatórias

        do na = 1,NA1

!       CALCULANDO peso de cada ATIVO
!       Escolhendo ordem dos ativos aleatóriamente
        vola = 0.0
        WVec = 0.0
        ISharpe = 0.0
        sumvola = 0.0
        pc = 1.0

        esco = 0
        jafoi = 0
        ctes = 0

        do i = 1,Ativos-1

654     continue
        esco = Ativos*grnd() + 1
        do kbt = 1,Ativos
        if(esco .eq. jafoi(1,kbt))then
        goto 654
        end if
        enddo

        ctes = ctes + 1
        jafoi(1,ctes) = esco

        WVec(1,esco) = pc*grnd()

        pc = pc - WVec(1,esco)

        enddo


        do kbt = 1,Ativos
        tem = 0
        do tbk = 1,Ativos
        if(jafoi(1,tbk) .eq. kbt)then
        tem = 1
        endif
        enddo

        if(tem .eq. 0)then
        WVec(1,kbt) = pc
        endif

        enddo


!       Retorno esperado da carteira
        EPoRe = 0.0
        do i = 1,Ativos
        EPoRe = EPoRe + MedRet(1,i)*periodo*WVec(1,i)
        enddo

!       volatilidade
        do i = 1,Ativos
        do j = 1,Ativos
        sumvola = sumvola + MCor(i,j)*WVec(1,i)*WVec(1,j)
        end do
        end do

        vola = sqrt(sumvola)

!       indice Sharpe
        ISharpe = (EPoRe - TaxaFixa)/vola

!       Escrevendo em arquivo
        write(13,*)WVec,sum(WVec),EPoRe,vola,ISharpe

        enddo



        write(*,*)"============================"
        write(*,*)"||  Retorno Médio diario,Anual   ||"
        write(*,*)"============================"

        print *,'ITUB3 = ',MedRet(1,1),MedRet(1,1)*periodo,'||'
        print *,'           ---------------------------'
        print *,'BBAS3 = ',MedRet(1,2),MedRet(1,2)*periodo,'||'
        print *,'           ---------------------------'
        print *,'PETR4 = ',MedRet(1,3),MedRet(1,3)*periodo,'||'
        print *,'           ---------------------------'
        print *,'VALE3 = ',MedRet(1,4),MedRet(1,4)*periodo,'||'
        print *,'           ---------------------------'
        print *,'MGLU3 = ',MedRet(1,5),MedRet(1,5)*periodo,'||'
        print *,'           ---------------------------'
        print *,'SUSB3 = ',MedRet(1,6),MedRet(1,6)*periodo,'||'
        print *,'           ---------------------------'


        print *,'Desvios: '
        print *,'ITUB3 = ',Desvio(1,1)
        print *,'BBAS3 = ',Desvio(1,2)
        print *,'PETR4 = ',Desvio(1,3)
        print *,'VALE3 = ',Desvio(1,4)
        print *,'MGLU3 = ',Desvio(1,5)
        print *,'SUSB3 = ',Desvio(1,6)


        print *,"FIM"
        pause

        end







!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      MERSENNE TWISTER RANDOM NUMBER GENERATOR


      subroutine sgrnd(seed)

      implicit integer(a-z)

      parameter(N     =  624)

      dimension mt(0:N-1)

      common /block/mti,mt
      save   /block/


      mt(0)= iand(seed,-1)
      do 1000 mti=1,N-1
        mt(mti) = iand(69069 * mt(mti-1),-1)
 1000 continue

      return
      end

      double precision function grnd()

      implicit integer(a-z)

      parameter(N     =  624)
      parameter(N1    =  N+1)
      parameter(M     =  397)
      parameter(MATA  = -1727483681)
      parameter(UMASK = -2147483648.0)
      parameter(LMASK =  2147483647)
      parameter(TMASKB= -1658038656)
      parameter(TMASKC= -272236544)

      dimension mt(0:N-1)
      common /block/mti,mt
      save   /block/
      data   mti/N1/

      dimension mag01(0:1)
      data mag01/0, MATA/
      save mag01
      TSHFTU(y)=ishft(y,-11)
      TSHFTS(y)=ishft(y,7)
      TSHFTT(y)=ishft(y,15)
      TSHFTL(y)=ishft(y,-18)

      if(mti.ge.N) then
        if(mti.eq.N+1) then
          call sgrnd(4357)
        endif

        do 1000 kk=0,N-M-1
            y=ior(iand(mt(kk),UMASK),iand(mt(kk+1),LMASK))
            mt(kk)=ieor(ieor(mt(kk+M),ishft(y,-1)),mag01(iand(y,1)))
 1000   continue
        do 1100 kk=N-M,N-2
            y=ior(iand(mt(kk),UMASK),iand(mt(kk+1),LMASK))
            mt(kk)=ieor(ieor(mt(kk+(M-N)),ishft(y,-1)),mag01(iand(y,1)))
 1100   continue
        y=ior(iand(mt(N-1),UMASK),iand(mt(0),LMASK))
        mt(N-1)=ieor(ieor(mt(M-1),ishft(y,-1)),mag01(iand(y,1)))
        mti = 0
      endif

      y=mt(mti)
      mti=mti+1
      y=ieor(y,TSHFTU(y))
      y=ieor(y,iand(TSHFTS(y),TMASKB))
      y=ieor(y,iand(TSHFTT(y),TMASKC))
      y=ieor(y,TSHFTL(y))

      if(y.le.0) then
        grnd=(dble(y)+2.0d0**32)/(2.0d0**32-1.0d0)
      else
        grnd=dble(y)/(2.0d0**32-1.0d0)
      endif
      if(grnd .ge. 1.0)then
        grnd = 0.99
      end if

      return
      end


