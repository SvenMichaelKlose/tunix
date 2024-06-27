; 6502 code generator description

(register a x y)
(declare char a x y)

(definst cpu description
  conditions
  inputs
  outputs)

(def (= a (const (byte x)))
  (+ "lda #" (out x t) (terpri t))

(def (= (byte (mem d)) (byte (mem s)))
  (= a (ref s))
  (side z (== a 0))
  (side n (< a 0))
  (= (ref d) a))

(def (= (byte (mem d)) a)
  (+ "sta " (out d t)(terpri t)))

(def (= (word (mem d)) (word (mem s)))
  (= (byte (mem d)) (byte (mem s)))
  (= (byte (mem (++ d))) (byte (mem (++ s)))))
