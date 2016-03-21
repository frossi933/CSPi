(*

CSP como tipo coinductivo

Trace := lista posiblemente infinita de Output
isTrace : CSP -> Trace -> Prop

Variable execAct : Evento -> Output. 
Variable inputSpec : Input -> nat -> bool

Parameter random : Estado -> Event

smallevalStep : Proc -> Event -> Proc

refine : forall p q t, smallevalStep p e q -> isTrace q t -> isTrace p (e : t)


eval : Proc -> Trace

correct_eval : forall P, isTrace P (eval P). 
*)

Load trace.

Inductive Menu : Set :=
  none : Menu
| some : Event -> Menu -> Menu.

Fixpoint concatm (m1 m2:Menu) : Menu :=
  match m1 with
    none => m2
  | some e m1' => some e (concatm m1' m2)
  end.

(* A = A [] B 
   B = B [] A 
CoFixpoint menu (p:Proc) : Menu :=
  match p with
    stop => none
  | skip => none
  | pref e r => some e none
  | choi p1 p2 => match menu p1, menu p2 with
                    none, none => none
                  | none, some e m => some e m
                  | some e1 m1, none => some e1 m1
                  | some e1 m1, some e2 m2 => some e1 (some e2 (concatm m1 m2))
                  end
  | l => none
  end.
*)

(*
Definition Conf : Set := Event -> bool. (* configuracion estatica del pred de cada evento *) 
Parameter conf : Conf.
Definition conftrue (e:Event) := true.
Parameter menu_conf : Proc -> Conf -> Menu.
Definition menu (p: Proc) : Menu := menu_conf p conftrue.
*)
Parameter menu : Proc -> Menu.

Fixpoint elemOfMenu (e:Event)(m:Menu) : bool := match m with
  none => false
| some e' m' => if beq_event e e' then true else elemOfMenu e m'
end.

CoFixpoint smallstep_eval (p:Proc) (e:Event) : Proc :=
  match p with 
    stop => stop
  | skip => skip
  | pref v r => if beq_event v e then r else pref v r
  | choi stop p2 => stop
  | choi skip skip => skip
  | choi skip p2 => choi skip (smallstep_eval p2 e)
  | choi p1 stop => stop
  | choi p1 skip => choi (smallstep_eval p1 e) skip
  | choi p1 p2 => if elemOfMenu e (menu p1) then choi (smallstep_eval p1 e) skip
                                            else if elemOfMenu e (menu p2) then choi skip (smallstep_eval p2 e)
                                                                           else choi p1 p2
  | seq skip skip => skip
  | seq skip p2 => seq skip (smallstep_eval p2 e)
  | seq stop p2 => stop
  | seq p1 p2 => seq (smallstep_eval p1 e) p2
  | par s skip skip => skip
  | par s skip p2 => if isElem e s then par s skip p2 else par s skip (smallstep_eval p2 e)
  | par s p1 skip => if isElem e s then par s p1 skip else par s (smallstep_eval p1 e) skip
  | par s p1 p2 => if (isElem e s) then (if andb (elemOfMenu e (menu p1)) (elemOfMenu e (menu p2)) 
                                         then par s (smallstep_eval p1 e) (smallstep_eval p2 e) (* sync *)
                                         else par s p1 p2)
                                   else (if elemOfMenu e (menu p1) then par s (smallstep_eval p1 e) p2
                                         else if elemOfMenu e (menu p2) then par s p1 (smallstep_eval p2 e)
                                              else par s p1 p2)
  end.

(* ver de probar este tipo de cosas ... *)
Lemma seqSkip : forall (p1 p2:Proc)(e:Event), OpSem p1 e p2 -> OpSem (seq skip p1) e p2.

Parameter ev:Event.
Eval compute in (match smallstep_eval stop ev with
                   stop => stop
                 | p => skip end).


(*****************************
   Demostracion de correctitud de small-step 
*****************************) 

Lemma sseval_stop : forall (e:Event), smallstep_eval stop e = stop.
Proof.
  intro e.
  proc_unfold (smallstep_eval stop e).
  simpl.
  auto.
Qed.

Lemma sseval_skip : forall (e:Event), smallstep_eval skip e = skip.
Proof.
  intro e.
  proc_unfold (smallstep_eval skip e).
  simpl;auto.
Qed.

Lemma sseval_pref_true : forall (e f:Event)(p:Proc), Beq_event e f -> smallstep_eval (pref e p) f = p.
Proof.
  intros e f p H.
  proc_unfold (smallstep_eval (pref e p) f).
  simpl.
  rewrite beq_event_true.
  destruct p;auto.
  assumption.
Qed.

Lemma sseval_pref_false : forall (e f:Event)(p:Proc), ~(Beq_event e f) -> smallstep_eval (pref e p) f = (pref e p).
Proof.
  intros e f p H.
  proc_unfold (smallstep_eval (pref e p) f).
  simpl.
  case_eq (beq_event e f);intro H1.
  apply eq_event_true in H1.
  contradiction.
  auto.  
Qed.

Lemma sseval_choi_stopl : forall (e:Event)(p:Proc), smallstep_eval (choi stop p) e = stop.
Proof.
  intros e p.
  proc_unfold (smallstep_eval (choi stop p) e);simpl.
  auto.  
Qed.

Lemma sseval_choi_stopr : forall (e:Event)(p:Proc), smallstep_eval (choi p stop) e = stop.
Proof.
  intros e p.
  proc_unfold (smallstep_eval (choi p stop) e);simpl.
  case p;simpl;auto.
  
Axiom choiskipskip : skip = choi skip skip.

Lemma sseval_choi_skip : forall (e:Event)(p:Proc), smallstep_eval (choi skip p) e = choi skip (smallstep_eval p e).
Proof.
  intros e p.
  proc_unfold (smallstep_eval (choi skip p) e);simpl.
  case_eq p;intro H1;simpl;auto.
  rewrite sseval_skip.
  exact choiskipskip.
Qed.


Lemma sseval_choi_skipp : forall (e:Event)(p q:Proc), OpSem p e q -> smallstep_eval (choi skip p) e = choi skip q.
Proof.
  intros e p q H.
  proc_unfold (smallstep_eval (choi skip p) e);simpl.
  inversion H.
  case p;simpl.






Theorem sseval_correct : forall (e:Event)(p:Proc), ~(NullProc p) -> OpSem p e (smallstep_eval p e).
Proof.
  cofix H.
  intros e p H1.
  case_eq p. 
  intro H2.
  rewrite H2 in H1.
  elim H1;constructor.
  intro H2.
  rewrite sseval_skip.
  constructor.
  intros e1 p1 H2.
  elim (classic_beq_event e1 e);intro H3.
  rewrite (sseval_pref_true e1 e p1 H3).
  constructor;assumption.
  rewrite (sseval_pref_false e1 e p1 H3).
  constructor;assumption.
  intros p0 p1 H2.
  case_eq p0.
   intro H3.
   rewrite H3 in H2;rewrite H2 in H1;elim H1;constructor;constructor.
   rewrite (sseval_choi_skip e p1).
   constructor.
   apply (H e p1).
   unfold not;intro H4.
   apply H1.
   rewrite H2;apply nullchoir;assumption.

   intros e0 p2 H4.
   
(* ill-formed

  exact (H e (choi p0 p1)).
  intros p0 p1.
  exact (H e (seq p0 p1)).
  intros e0 p0 p1.
  exact (H e (par e0 p0 p1)).
Qed.
*)


(*********************************
 Fin Demostracion 
*********************************)

Theorem ss_refine : forall (p q:Proc)(t:Trace)(e:Event), OpSem p e q -> isTrace t q -> isTrace ((execAct e) :: t) p.
Proof.
  intros p q t e H1 H2.
  case_eq p;intro H3.
  rewrite H3 in H1.
  inversion H1.
  


Parameter tick : Output.
Parameter empty : Output.
Parameter rand : unit -> bool. (* ?? *)

Fixpoint choose (m:Menu) : option Event := match m with
  none => None
| some e none => Some e
| some e es => if rand tt then Some e
                            else choose es
end.

CoFixpoint eval (p:Proc) : Trace := match p with
  stop => nil
| skip => tick :: nil
| p' => match choose (menu p') with
          None => nil
        | Some (EIn id pr as e) => empty :: eval (smallstep_eval p e)
        | Some (EOut id act as e) => execAct e :: eval (smallstep_eval p e)
        end
end.


Theorem eval_correct : forall (p:Proc), isTrace (eval p) p.
Proof.