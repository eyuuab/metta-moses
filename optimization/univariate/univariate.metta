; Generates an initial population (list of scored instances).
; Each instance has a random structure and is assigned the worst initial score.
; Params:
;   $instLength — length of each instance.
;   $nGenerate — number of instances to generate.
; Returns: List of mkSInst objects representing the population.
(: generateInitialSample (-> Number Number List))
(= (generateInitialSample $instLength $nGenerate)
   (if (== $nGenerate 0)
       Nil
       (Cons (mkSInst (mkPair (mkInst (generateRandomInstance $instLength)) (worstCscore))) (generateInitialSample $instLength (- $nGenerate 1)))))
