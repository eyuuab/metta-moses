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

; Collects field values for all knob indices across all instances.
; Calls getNthInstFields for each knob position and accumulates the results.
; Params:
;   $instsExp — list of instance expressions ((inst score) ...).
;   $instLength — number of knobs (fields) per instance.
;   $noInsts — total number of instances.
;   $n — current knob index (loop counter).
;   $acc — accumulator for tuples of field values.
; Returns: List of lists, where each sublist contains all values for one knob position.
(: instsToFieldValues (-> Expression Number Number Number Expression Expression))
(= (instsToFieldValues $instsExp $instLength $noInsts $n $acc)
     (if (== $n $instLength)
         $acc
         (let* (
             ($nthFields (getNthInstFields $instsExp $noInsts 0 $n ()))
             ($rest (instsToFieldValues $instsExp $instLength $noInsts (+ $n 1) $acc))
           )
           (cons-atom $nthFields $rest)
         )
     )
)

; Learns the frequency of each possible field value (0, 1, 2) for every knob (field) across all instances.
; Used later to derive probability-based sampling distributions.
; Params:
;   $fieldVals — list of tuples; each tuple contains all field values for one knob index across all instances.
;   $acc — accumulator for probability counts.
; Returns: List of tuples (count0, count1, count2) for each field position.
; (: learnProbability (-> Expression Expression Expression))
(= (learnProbability $fieldVals $acc)
   (if (== $fieldVals ())
       $acc
       (let* (
           ($fieldVal (car-atom $fieldVals))
           ($count0 (countN $fieldVal 0 0))
           ($count1 (countN $fieldVal 1 0))
           ($count2 (countN $fieldVal 2 0))
           ($rest (learnProbability (cdr-atom $fieldVals) $acc))
         )
         (cons-atom ($count0 $count1 $count2) $rest)
       )
   )
)

; Selects one field value (0, 1, or 2) using roulette-wheel selection.
; The selection is proportional to the frequency of each field value.
; Params:
;   $fieldCount — tuple of counts (count0, count1, count2) for one knob.
;   $tot — total sum of all counts (used to scale random selection).
;   $ctr — index counter (initially 0).
; Returns: The selected field index (0, 1, or 2).
; (: roulette_select (-> Expression Number Number Number))
(= (roulette_select $fieldCount $tot $ctr)
(if (== $fieldCount ()) (- $ctr 1)
(let*
  (
    ($rndfloat (randomFloat))
    ($ajstdSum (* $tot $rndfloat))
  )
  (if (<= (- $ajstdSum (car-atom $fieldCount)) 0) $ctr
      (roulette_select (cdr-atom $fieldCount) $tot (+ $ctr 1)))
)))

; Samples a single instance based on the learned probability model.
; For each knob, selects one value using roulette selection.
; Params:
;   $probabilityModel — list of tuples [(count0, count1, count2), ...]
;                            representing each knob’s distribution.
;   $tot — total count sum for normalization in roulette selection.
;   $acc — accumulator for the selected field values.
; Returns: A list representing one sampled instance (e.g., (2 0 1)).
; (: sample (-> Expression Number Expression Expression))
(= (sample $probabilityModel $tot $acc)
   (if (== $probabilityModel ())
       $acc
       (let* (
           ($fieldCount (car-atom $probabilityModel))
           ($selectedFieldVal (roulette_select $fieldCount $tot 0))
           ($rest (sample (cdr-atom $probabilityModel) $tot $acc))
         )
         (cons-atom $selectedFieldVal $rest)
       )
   )
)

; Generates multiple sampled instances using the probability model.
; Each instance is generated by 'sample' and given a default (worst) score.
; Params: 
;   $probabilityModel — learned probability model for all knobs.
;   $nGenerate — number of instances to generate.
;   $tot — total count sum used in selection.
;   $acc — accumulator for the generated scored instances.
; Returns: List of scored instances: [((inst) (worstCscore)), ...].
; (: sampleInsts (-> Expression Number Number Expression Expression))
(= (sampleInsts $probabilityModel $nGenerate $tot $acc)
(if (== $nGenerate 0) $acc
    (let $sampledInst (sample $probabilityModel $tot ())
         (sampleInsts $probabilityModel (- $nGenerate 1) $tot (cons-atom ($sampledInst (worstCscore)) $acc))
)))