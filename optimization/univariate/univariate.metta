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

; Recursively performs tournament comparison to pick the best instance.
; Randomly selects another instance and keeps the better one based on score.
; Params:
;   $instSet — full set of instances.
;   $res — current best candidate.
;   $tSize — remaining number of comparisons to perform.
; Returns: The best mkSInst found after all comparisons.
(: tournamentCompare (-> (InstanceSet $score) (ScoredInstance $score) Number (ScoredInstance $score)))
(= (tournamentCompare (mkSInstSet $instSet) $res $tSize)
(if (== $tSize 0) $res
(let*
  (
    ($instSetLen (List.length $instSet))
    ($tmp (List.getByIdx $instSet (random-int 0 $instSetLen)))
    ($updatedRes (if (cScore>= (getSInstScore $tmp) (getSInstScore $res)) $tmp $res))
  )
  (tournamentCompare (mkSInstSet $instSet) $updatedRes (- $tSize 1)))))

; Selects the best individuals from a set using tournament selection.
; Randomly samples $tSize individuals and keeps the best one, repeating $nSelect times.
; Params:
;   $instSet — the full set of scored instances.
;   $tSize — tournament size (number of competitors per selection).
;   $nSelect — number of individuals to select.
;   $selectedList — accumulator list for results (initially Nil).
; Returns: List of selected mkSInst objects.
(: tournamentSelection (-> (InstanceSet $score) Number Number List List))
(= (tournamentSelection (mkSInstSet $instSet) $tSize $nSelect $selectedList)
(if (== $nSelect 0) $selectedList
(let*
  (
    (() (println! "Tournament selection, remaining to select: " $nSelect))
    ($instSetLen (List.length $instSet))
    (() (println! "Instance set length: " $instSetLen))
    ($res (List.getByIdx $instSet (random-int 0 $instSetLen)))
    (() (println! "Initial random candidate: " $res))
    ($updatedRes (tournamentCompare (mkSInstSet $instSet) $res (- $tSize 1)))
    (() (println! "Selected candidate: " $updatedRes))

  )
  (tournamentSelection (mkSInstSet $instSet) $tSize (- $nSelect 1) (Cons $updatedRes $selectedList)))))

; Extracts the value of the nth knob (field) from each instance expression.
; Builds a list of the values at the same index position across all instances.
; Params:
;   $instsExp — list of instance expressions ((inst score) ...).
;   $noInsts — total number of instances.
;   $nthInstCtr — current instance index (loop counter).
;   $nthKnobCtr — index of the knob/field to extract.
;   $acc — accumulator for collected field values.
; Returns: List of field values for the specified knob position.
(: getNthInstFields (-> Expression Number Number Number Expression Expression))
(= (getNthInstFields $instsExp $noInsts $nthInstCtr $nthKnobCtr $acc)
   (if (== $nthInstCtr $noInsts)
       $acc
       (let* (
           ($nthInst (index-atom $instsExp $nthInstCtr))
           (($inst $score) $nthInst)
           ($nthKnob (index-atom $inst $nthKnobCtr))
           ($rest (getNthInstFields $instsExp $noInsts (+ $nthInstCtr 1) $nthKnobCtr $acc))
         )
         (cons-atom $nthKnob $rest)
       )
   )
)