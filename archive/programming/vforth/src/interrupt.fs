vocabulary interrupt immediate
interrupt definitions

hex

( IRQ vector )
314 constant cinv

( set interrupt word )
code irqw!  ( cfa  --  )
    bot 1+ lda,  irqw 1+ sta,  bot lda,  irqw sta,  pop jmp,
    end-code

variable olds
0  olds  !

( IRQ service routine )
code irqs
    0 # lda,  irqnot sta,  olds ) jmp,
    end-code

( set interrupt vector )
code setup  (  --  )
    olds lda,  0= if,
        sei,  cinv 1+ lda,  olds 1+ sta,
        cinv lda,  olds sta,
        ' irqs >body 100 /mod  # lda,  cinv 1+ sta,  # lda,  cinv sta,
        cli,
    then,  next jmp,
    end-code

( restore original interrupt vector )
code restore  (  --  )
    olds lda,  0= not if,
        sei,  olds 1+ lda,  cinv 1+  sta,
        olds lda,  cinv sta,
        0 # lda,  olds sta,  cli,
    then,  next jmp,
    end-code

' restore  ex-error  catch

forth definitions
decimal
