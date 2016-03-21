Load events.

(* Expresiones del lenguaje CSP *)
CoInductive Proc : Set := 
  stop : Proc
| skip : Proc
| pref : Event -> Proc -> Proc
| choi : Proc -> Proc -> Proc
| seq :  Proc -> Proc -> Proc
| par :  EvSet -> Proc -> Proc -> Proc.

Definition Proc_decompose (e:Proc) : Proc :=
  match e with
    stop => stop
  | skip => skip
  | pref v p => pref v p
  | choi m p => choi m p
  | seq m p => seq m p
  | par s m p => par s m p
  end.

Lemma Proc_decompose_lemma : forall p:Proc, p = Proc_decompose p.
Proof.
  intro e.
  case e;trivial.
Qed.

Ltac proc_unfold term := apply trans_equal with (1:=Proc_decompose_lemma term).


(* Parte de la Semantica Operacional para CSP 

CoInductive OpSem : Proc -> Event -> Proc -> Prop :=
| stop_sem:     forall (e:Event), OpSem stop e stop
| skip_sem:     forall (e:Event), OpSem skip e skip.
| not_pref_sem: forall (p:Proc)(e f:Event), ~(Beq_event f e) -> OpSem (pref f p) e (pref f p)
| pref_sem:     forall (p:Proc)(e f:Event), Beq_event f e -> OpSem (pref f p) e p
| choi1_sem:    forall (p q r:Proc)(e:Event), OpSem p e r -> OpSem (choi p q) e r
| choi2_sem:    forall (p q r:Proc)(e:Event), OpSem q e r -> OpSem (choi p q) e r
| par1: forall (p q r:Proc)(e:Event), OpSem p e r -> OpSem (par p q) e (par r q)
| sem5: forall (p q r:Proc)(e:Event), OpSem q e r -> OpSem (par p q) e (par p r)
| sem6: forall (p q r:Proc)(e:Event), OpSem p e r -> OpSem (seq p q) e (seq r q)
| sem7: forall (p r:Proc)(e:Event), OpSem p e r -> OpSem (seq skip p) e r (* ?? *)
| sem7a: forall (p r:Proc)(e:Event), OpSem (seq skip p) e p (* ?? *)
| sem7b: forall (p r:Proc)(e:Event), OpSem p e skip -> OpSem (seq p r) e r (* ?? *)
| sem8: forall (p :Proc)(e:Event), OpSem (seq stop p) e stop


  skip =(e)=> skip   ????????????
 
  (e -> p) =(e)=> p              

  (e -> p) =(f)=> (e -> p)

  (skip [] skip) =(e)=> skip

            p =(e)=> q           
 --------------------------------
  (p [] skip) =(e)=> (q [] skip) 

            p =(e)=> q           
 --------------------------------
  (skip [] p) =(e)=> (skip [] q) 

            p =(e)=> r
 --------------------------------
     (p [] q) =(e)=> (r [] skip)

            q =(e)=> r
 --------------------------------
     (p [] q) =(e)=> (skip [] r)

  (skip ; skip) =(e)=> skip

           p =(e)=> q
 ------------------------------
  (skip ; p) =(e)=> (skip ; q)

        p =(e)=> r
 ------------------------
  (p ; q) =(e)=> (r ; q)


*)
CoInductive OpSem : Proc -> Event -> Proc -> Prop :=
(*| stop_sem:     forall (e:Event), OpSem stop e stop*)
| skip_sem:     forall (e:Event), OpSem skip e skip
| not_pref_sem: forall (p:Proc)(e f:Event), ~(Beq_event f e) -> OpSem (pref f p) e (pref f p)
| pref_sem:     forall (p:Proc)(e f:Event), Beq_event f e -> OpSem (pref f p) e p
| choi_skipl:   forall (p q:Proc)(e:Event), OpSem p e q -> OpSem (choi skip p) e (choi skip q)
| choi_skipr:   forall (p q:Proc)(e:Event), OpSem p e q -> OpSem (choi p skip) e (choi q skip)
| choi_stopl:   forall (p q:Proc)(e:Event), OpSem p e stop -> OpSem (choi p q) e stop
| choi_stopr:   forall (p q:Proc)(e:Event), OpSem p e stop -> OpSem (choi q p) e stop 
| choi_left:    forall (p q r:Proc)(e:Event), OpSem p e r -> OpSem (choi p q) e (choi r skip)
| choi_right:   forall (p q r:Proc)(e:Event), OpSem q e r -> OpSem (choi p q) e (choi skip r)
| seq_skips:    forall (e:Event), OpSem (seq skip skip) e skip (* axioma o alguna equivalencia *)
| seq_left:     forall (p q r:Proc)(e:Event), OpSem p e r -> OpSem (seq p q) e (seq r q)
| seq_right:    forall (p q:Proc)(e:Event), OpSem p e q -> OpSem (seq skip p) e (seq skip q)
| seq_skipstop: forall (p q:Proc)(e:Event), OpSem p e stop -> OpSem (seq skip p) e stop (* contemplado en el caso anterior *)
| seq_stop:     forall (p q:Proc)(e:Event), OpSem p e stop -> OpSem (seq p q) e stop (* contemplado en seq_left *)
| par_skips:    forall (e:Event)(s:EvSet), OpSem (par s skip skip) e skip (* axioma o alguna equivalencia *)
| par_sync:     forall (p q r t:Proc)(e:Event)(s:EvSet), isMember e s -> OpSem p e q -> OpSem r e t -> OpSem (par s p r) e (par s q t)
| par_left:     forall (p q r:Proc)(e:Event)(s:EvSet), ~(isMember e s) -> OpSem p e r -> OpSem (par s p q) e (par s r q)
| par_right:    forall (p q r:Proc)(e:Event)(s:EvSet), ~(isMember e s) -> OpSem q e r -> OpSem (par s p q) e (par s p r).
(* evset debe contener todos los eventos en comun entre p y q, sino no se asegura el buen comportamiento *)

CoInductive NullProc : Proc -> Prop :=
  nullstop : NullProc stop
| nullchoil : forall p q:Proc, NullProc q -> NullProc (choi q p)
| nullchoir : forall p q:Proc, NullProc q -> NullProc (choi p q)
| nullseq : forall p q:Proc, NullProc q -> NullProc (seq q p)
| nullseq2 : forall p:Proc, NullProc p -> NullProc (seq skip p)
| nullparl : forall (p q:Proc)(s:EvSet), NullProc q -> NullProc (par s q p)
| nullparr : forall (p q:Proc)(s:EvSet), NullProc q -> NullProc (par s p q).