C
C     Declaracoes
C

      real*8 rnum(20000),pmod,dmax,a,b,alfa,med,ps,tls
      real*8 tec(2000),ts(2000),tr(2000),tini,tfim(2000),wq,tsis,lq,ls
      real*8 stls,stec,sts,swq,nwait,stsis,r1,r2,r3,r4,r5,r6
      integer iseed,num,gerar,eCheg,sCheg,eSaid,sSaid,m,is

C      open(2, file= 'sim1.b.csv',status = 'unknown')
      open(1, file= 'r11.dat',status = 'unknown')
      open(2, file= 'r12.dat',status = 'unknown')
      open(3, file= 'r13.dat',status = 'unknown')
      open(4, file= 'r14.dat',status = 'unknown')
      open(5, file= 'r15.dat',status = 'unknown')
      open(6, file= 'r16.dat',status = 'unknown')
      open(7, file= 'res.csv',status = 'unknown')
      open(21, file= 'r21.dat',status = 'unknown')
      open(22, file= 'r22.dat',status = 'unknown')
      open(23, file= 'r23.dat',status = 'unknown')
      open(24, file= 'r24.dat',status = 'unknown')
      open(25, file= 'r25.dat',status = 'unknown')
      open(26, file= 'r26.dat',status = 'unknown')
      open(27, file= 'res2.csv',status = 'unknown')
      open(31, file= 'r31.dat',status = 'unknown')
      open(32, file= 'r32.dat',status = 'unknown')
      open(33, file= 'r33.dat',status = 'unknown')
      open(34, file= 'r34.dat',status = 'unknown')
      open(35, file= 'r35.dat',status = 'unknown')
      open(36, file= 'r36.dat',status = 'unknown')
      open(37, file= 'res3.csv',status = 'unknown')
C      open(8, file= 'sim1.csv',status = 'unknown')
C
C     Formatos
C

C8    format('Semente')
C9    format(I5)
C10   format('Gerar quantos aleatorios')
C11   format(F10.5,F10.5)
C12   format(F10.5)
13    format(I5,',',F15.10,',',F15.10,',',F15.10,',',F15.10,',',F15.10,'
     *,',F15.10,',',F15.10,',',F15.10,',',F15.10,',',F15.10,',',F15.10)
14    format(F15.10,',',F15.10)

15    format('Tempo,*Intervalo de Chegada (TEC),*Tempo de Atendimento (T
     *S),Instante de Chegada (T Real),Instante de Atendimento (T Inicio)
     *,Instante de Saida (T Fim),Tempo em Espera (Wq),Tempo Total (Ws) (
     *T Sistema),Pessoas na Fila na Chegada (Lq),Pessoas no Sistema na C
     *hegada (Ls),Pessoas na Fila na Saida (PS),Tempo de Sistema Livre (
     *TLS)')
16    format(F15.10,',',F15.10,',',F15.10,',',F15.10)
17    format(I5,',',F15.9,',',F15.9,',',F15.9,',',F15.9,',',F15.9,',',F1
     *5.9,',',F15.9)
18    format(I5,',',F15.10)
C15   format('Fim')
C16   format(I5,F15.10)

C
C     Entrada
C

      is = 1
      num = 1000
      gerar = 500

C
C     Inicializacao     
C

      tini = 0
      wq = 0
      tsis = 0
      lq = 0
      ls = 0
      ps = 0
      tls = 0
      stls = 0
      stec = 0
      sts = 0
      swq = 0
      nwait = 0
      stsis = 0
      r1 = 0
      r2 = 0
      r3 = 0
      r4 = 0
      r5 = 0
      r6 = 0
      
      do i=1,gerar
          tec(i) = 0
          ts(i) = 0
          tr(i) = 0
          tfim(i) = 0
      end do
      
      do i=1,num
          rnum(i) = 0.0D0
      end do

C      write(1,15)

C
C     Calculo
C

      do is=1,8000
        iseed = is
        pmod = 2147483647.D0
        dmax = 1.0D0/pmod
        num = num + 1

C       mudando o iseed

        rnum(1) = iseed * dmax
        do i=2,num
           rnum(i) = cong16807(iseed)
        end do

C       Calculo com intervalo normal

        a = 10
        b = 20

        alfa = 4.0D0/60.0D0
        med = 1/alfa

        do i=1,gerar
            tec(i)=-log(rnum(i+2))/alfa
            ts(i)=(b-a)*rnum(i+2)+a
            if (i == 1) then
                tr(i) = tec(i)
                tini = tec(i)
                tfim(i) = ts(i) + tini
                wq = 0
                lq = 0
                ls = 1
                tls = tec(i)
            else
                tr(i) = tr(i-1) + tec(i)
                if (tr(i) <= tfim(i-1)) then
                    tini = tfim(i-1)
                    wq = tfim(i-1) - tr(i)
                else
                    tini = tr(i)
                    wq = 0
                end if
                tfim(i) = ts(i) + tini
                if (tr(i) >= tfim(i-1)) then
                    tls = tr(i) - tfim(i-1)
                else
                    tls = 0
                end if
            end if

            tsis = tfim(i) - tr(i)
            stls = stls + tls
            stec = stec + tec(i)
            sts = sts + ts(i)
            swq = swq + wq
            if (wq == 0.0D0) then
               nwait = nwait + 1.0D0
            end if
            stsis = stsis + tsis
C        write(8,13)is,tec(i),ts(i),tr(i),tini,tfim(i),wq,tsis,0,0,0,tls
        end do

        do k=1,gerar
           eCheg = 0
           sCheg = 0
           eSaid = 0
           sSaid = 0
           do j=1,gerar
           if (tr(j) <= tr(k)) then
              eCheg = eCheg + 1.0D0
           end if
           if (tfim(j) <= tr(k)) then
              sCheg = sCheg + 1.0D0
           end if
           if (tr(j) <= tfim(k)) then
              eSaid = eSaid + 1.0D0
           end if
           if (tfim(j) <= tfim(k)) then
              sSaid = sSaid + 1.0D0
           end if
           end do
           lq = eCheg - sCheg - 1.0D0
           ls = lq + 1.0D0
           ps = eSaid - sSaid

C          write(2,16) lq,ls,ps
        end do

        r1 = stls/tfim(gerar)
        r2 = stec/gerar
        r3 = sts/gerar
        r4 = swq/gerar
        r5 = nwait/gerar
        r6 = stsis/gerar

        write(1,18)is,r1
        write(2,18)is,r2
        write(3,18)is,r3
        write(4,18)is,r4
        write(5,18)is,r5
        write(6,18)is,r6
        write(7,17)is,r1,r2,r3,r4,r5,r6,tfim(gerar)

        tini = 0
        wq = 0
        tsis = 0
        lq = 0
        ls = 0
        ps = 0
        tls = 0
        stls = 0
        stec = 0
        sts = 0
        swq = 0
        nwait = 0
        stsis = 0
        r1 = 0
        r2 = 0
        r3 = 0
        r4 = 0
        r5 = 0
        r6 = 0

        do m=1,gerar
            tec(m) = 0
            ts(m) = 0
            tr(m) = 0
            tfim(m) = 0
        end do

C       Calculo com intervalo dobrado

        a = 20
        b = 40

        alfa = 4.0D0/60.0D0
        med = 1/alfa

        do i=1,gerar
            tec(i)=-log(rnum(i+2))/alfa
            ts(i)=(b-a)*rnum(i+2)+a
            if (i == 1) then
                tr(i) = tec(i)
                tini = tec(i)
                tfim(i) = ts(i) + tini
                wq = 0
                lq = 0
                ls = 1
                tls = tec(i)
            else
                tr(i) = tr(i-1) + tec(i)
                if (tr(i) <= tfim(i-1)) then
                    tini = tfim(i-1)
                    wq = tfim(i-1) - tr(i)
                else
                    tini = tr(i)
                    wq = 0
                end if
                tfim(i) = ts(i) + tini
                if (tr(i) >= tfim(i-1)) then
                    tls = tr(i) - tfim(i-1)
                else
                    tls = 0
                end if
            end if

            tsis = tfim(i) - tr(i)
            stls = stls + tls
            stec = stec + tec(i)
            sts = sts + ts(i)
            swq = swq + wq
            if (wq == 0.0D0) then
               nwait = nwait + 1.0D0
            end if
            stsis = stsis + tsis
C        write(8,13)is,tec(i),ts(i),tr(i),tini,tfim(i),wq,tsis,0,0,0,tls
        end do

        do k=1,gerar
           eCheg = 0
           sCheg = 0
           eSaid = 0
           sSaid = 0
           do j=1,gerar
           if (tr(j) <= tr(k)) then
              eCheg = eCheg + 1.0D0
           end if
           if (tfim(j) <= tr(k)) then
              sCheg = sCheg + 1.0D0
           end if
           if (tr(j) <= tfim(k)) then
              eSaid = eSaid + 1.0D0
           end if
           if (tfim(j) <= tfim(k)) then
              sSaid = sSaid + 1.0D0
           end if
           end do
           lq = eCheg - sCheg - 1.0D0
           ls = lq + 1.0D0
           ps = eSaid - sSaid

C          write(2,16) lq,ls,ps
        end do

        r1 = stls/tfim(gerar)
        r2 = stec/gerar
        r3 = sts/gerar
        r4 = swq/gerar
        r5 = nwait/gerar
        r6 = stsis/gerar

        write(21,18)is,r1
        write(22,18)is,r2
        write(23,18)is,r3
        write(24,18)is,r4
        write(25,18)is,r5
        write(26,18)is,r6
        write(27,17)is,r1,r2,r3,r4,r5,r6,tfim(gerar)

        tini = 0
        wq = 0
        tsis = 0
        lq = 0
        ls = 0
        ps = 0
        tls = 0
        stls = 0
        stec = 0
        sts = 0
        swq = 0
        nwait = 0
        stsis = 0
        r1 = 0
        r2 = 0
        r3 = 0
        r4 = 0
        r5 = 0
        r6 = 0

        do m=1,gerar
            tec(m) = 0
            ts(m) = 0
            tr(m) = 0
            tfim(m) = 0
        end do

C       Calculo com intervalo reduzido na metade

        a = 5
        b = 10

        alfa = 4.0D0/60.0D0
        med = 1/alfa

        do i=1,gerar
            tec(i)=-log(rnum(i+2))/alfa
            ts(i)=(b-a)*rnum(i+2)+a
            if (i == 1) then
                tr(i) = tec(i)
                tini = tec(i)
                tfim(i) = ts(i) + tini
                wq = 0
                lq = 0
                ls = 1
                tls = tec(i)
            else
                tr(i) = tr(i-1) + tec(i)
                if (tr(i) <= tfim(i-1)) then
                    tini = tfim(i-1)
                    wq = tfim(i-1) - tr(i)
                else
                    tini = tr(i)
                    wq = 0
                end if
                tfim(i) = ts(i) + tini
                if (tr(i) >= tfim(i-1)) then
                    tls = tr(i) - tfim(i-1)
                else
                    tls = 0
                end if
            end if

            tsis = tfim(i) - tr(i)
            stls = stls + tls
            stec = stec + tec(i)
            sts = sts + ts(i)
            swq = swq + wq
            if (wq == 0.0D0) then
               nwait = nwait + 1.0D0
            end if
            stsis = stsis + tsis
C        write(8,13)is,tec(i),ts(i),tr(i),tini,tfim(i),wq,tsis,0,0,0,tls
        end do

        do k=1,gerar
           eCheg = 0
           sCheg = 0
           eSaid = 0
           sSaid = 0
           do j=1,gerar
           if (tr(j) <= tr(k)) then
              eCheg = eCheg + 1.0D0
           end if
           if (tfim(j) <= tr(k)) then
              sCheg = sCheg + 1.0D0
           end if
           if (tr(j) <= tfim(k)) then
              eSaid = eSaid + 1.0D0
           end if
           if (tfim(j) <= tfim(k)) then
              sSaid = sSaid + 1.0D0
           end if
           end do
           lq = eCheg - sCheg - 1.0D0
           ls = lq + 1.0D0
           ps = eSaid - sSaid

C          write(2,16) lq,ls,ps
        end do

        r1 = stls/tfim(gerar)
        r2 = stec/gerar
        r3 = sts/gerar
        r4 = swq/gerar
        r5 = nwait/gerar
        r6 = stsis/gerar

        write(31,18)is,r1
        write(32,18)is,r2
        write(33,18)is,r3
        write(34,18)is,r4
        write(35,18)is,r5
        write(36,18)is,r6
        write(37,17)is,r1,r2,r3,r4,r5,r6,tfim(gerar)

        tini = 0
        wq = 0
        tsis = 0
        lq = 0
        ls = 0
        ps = 0
        tls = 0
        stls = 0
        stec = 0
        sts = 0
        swq = 0
        nwait = 0
        stsis = 0
        r1 = 0
        r2 = 0
        r3 = 0
        r4 = 0
        r5 = 0
        r6 = 0

        do m=1,gerar
            tec(m) = 0
            ts(m) = 0
            tr(m) = 0
            tfim(m) = 0
        end do

        do m=1,num
            rnum(m) = 0.0D0
        end do
      end do

      close(1)
      close(2)

      end

      function cong16807(iseed)
        integer iseed,imod
        real*8 rmod,pmod,dmax
        rmod = dfloat(iseed)
        pmod = 2147483647.0D0
        dmax = 1.0D0/pmod
        rmod = rmod*16807.D0
        imod = rmod * dmax
        rmod = rmod - pmod* imod
        cong16807 = rmod * dmax
        iseed = rmod
        return
      end

