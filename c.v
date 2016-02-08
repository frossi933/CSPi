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

Definition Id : Set := nat.
Definition Input : Set := bool.
Parameter Output : Set.
(*Definition Output : Set := nat.
Parameter Pred : nat -> Input.
Parameter Act : nat -> Output.
*)
Definition Pred : Set := nat -> Input.
Definition Act : Set := nat -> Output.

Inductive Event : Set :=
  EIn : Id -> Pred -> Event
| EOut : Id -> Act -> Event.

Variable execAct : Event -> Output.

(* Expresiones del lenguaje CSP *)
CoInductive Proc : Set := 
  stop : Proc
| skip : Proc
| pref : Event -> Proc -> Proc
| choi : Proc -> Proc -> Proc
| seq : Proc -> Proc -> Proc
| par :  Proc -> Proc -> Proc.

Definition Proc_decompose (e:Proc) : Proc :=
  match e with
    stop => stop
  | skip => skip
  | pref v p => pref v p
  | choi m p => choi m p
  | seq m p => seq m p
  | par m p => par m p
  end.

Lemma Proc_decompose_lemma : forall p:Proc, p = Proc_decompose p.
Proof.
  intro e.
  case e;trivial.
Qed.

Ltac proc_unfold term := apply trans_equal with (1:=Proc_decompose_lemma term).

CoInductive Trace : Set :=
  nil : Trace
| cons : Output -> Trace -> Trace.

Notation "e :: t" := (cons e t).

(* Successful termination or Finite Trace *)
CoInductive succTerm : Proc -> Prop :=
  st_skip : succTerm skip
| st_pref : forall (e:Event)(p:Proc), succTerm p -> succTerm (pref e p)
| st_choi : forall (p1 p2:Proc), succTerm p1 -> succTerm p2 -> succTerm (choi p1 p2)
| st_seq : forall (p1 p2:Proc), succTerm p1 -> succTerm p2 -> succTerm (seq p1 p2)
| st_par : forall (p1 p2:Proc), succTerm p1 -> succTerm p2 -> succTerm (par p1 p2).


CoFixpoint concatt (t1 t2:Trace) : Trace := match t1 with
  nil => t2
| cons o t1' => cons o (concatt t1' t2)
end.

CoInductive MergeTrace : Trace -> Trace -> Trace -> Prop :=
  mt_nil1: forall t:Trace, MergeTrace nil t t
| mt_cons1: forall (o:Output)(t1 t2 t3:Trace), MergeTrace t1 t2 t3 -> 
                                               MergeTrace (cons o t1) t2 (cons o t3)
| mt_nil2: forall t:Trace, MergeTrace t nil t
| mt_cons2: forall (o:Output)(t1 t2 t3:Trace), MergeTrace t1 t2 t3 -> 
                                               MergeTrace t1 (cons o t2) (cons o t3).
CoInductive isTrace : Trace -> Proc -> Prop :=
  nilt : forall p:Proc, isTrace nil p (* ?? *)
| preft : forall (t:Trace)(p:Proc)(e:Event), isTrace t p -> isTrace ((execAct e)::t) (pref e p)
| choit1 : forall (t1 t2:Trace)(p1 p2:Proc), isTrace t1 p1 -> isTrace t2 p2 -> isTrace t1 (choi p1 p2)
| choit2 : forall (t1 t2:Trace)(p1 p2:Proc), isTrace t1 p1 -> isTrace t2 p2 -> isTrace t2 (choi p1 p2)
| seqt: forall (t1 t2:Trace)(p1 p2:Proc), isTrace t1 p1 -> succTerm p1 -> isTrace t2 p2 -> isTrace (concatt t1 t2) (seq p1 p2)
| part: forall (t1 t2 t3:Trace)(p1 p2:Proc), isTrace t1 p1 -> isTrace t2 p2 -> MergeTrace t1 t2 t3 -> isTrace t3 (par p1 p2).


(* Parte de la Semantica Operacional para CSP *)
CoInductive OpSem : Proc -> Event -> Proc -> Prop :=
  sem1: forall (p:Proc)(e:Event), OpSem (pref e p) e p
| sem2: forall (p q r:Proc)(e:Event), OpSem p e r -> OpSem (choi p q) e r
| sem3: forall (p q r:Proc)(e:Event), OpSem q e r -> OpSem (choi p q) e r
| sem4: forall (p q r:Proc)(e:Event), OpSem p e r -> OpSem (par p q) e (par r q)
| sem5: forall (p q r:Proc)(e:Event), OpSem q e r -> OpSem (par p q) e (par p r)
| sem6: forall (p q r:Proc)(e:Event), OpSem p e r -> OpSem (seq p q) e (seq r q)
| sem7: forall (p r:Proc)(e:Event), OpSem p e r -> OpSem (seq skip p) e r (* ?? *)
| sem7a: forall (p r:Proc)(e:Event), OpSem (seq skip p) e p (* ?? *)
| sem7b: forall (p r:Proc)(e:Event), OpSem p e skip -> OpSem (seq p r) e r (* ?? *)
| sem8: forall (p :Proc)(e:Event), OpSem (seq stop p) e stop
| sem9: forall (e:Event), OpSem stop e stop
| sem10:forall (e:Event), OpSem skip e skip.

Parameter e : Event.
CoFixpoint a:Proc := pref e a.

Require Import Coq.Arith.EqNat.

(*Parameter beq_event : Event -> Event -> bool.
Axiom event_ref : forall e:Event, beq_event e e = true.
*)
Inductive Beq_event : Event -> Event -> Prop :=
  beq_in : forall (i1 i2:Id)(p1 p2:Pred), i1 = i2 -> Beq_event (EIn i1 p1)(EIn i2 p2)
| beq_out : forall (i1 i2:Id)(a1 a2:Act), i1 = i2 -> Beq_event (EOut i1 a1)(EOut i2 a2).

Definition beq_event (e1 e2: Event) : bool := match e1, e2 with
  EIn i1 p1 , EIn i2 p2 => beq_nat i1 i2
| EOut i1 a1 , EOut i2 a2 => beq_nat i1 i2
| _ , _ => false end.

Lemma eq_event_ref : forall e:Event, beq_event e e = true.
Proof.
  intro e.
  induction e;symmetry;exact (beq_nat_refl i).
Qed.

Theorem eq_event_true : forall (e1 e2:Event), beq_event e1 e2 = true -> Beq_event e1 e2.
Proof.
  intros e1 e2 H.
  induction e1;induction e2.
  constructor.
  simpl in H.
  apply beq_nat_true.
  auto.
  inversion H.
  inversion H.
  constructor.
  apply beq_nat_true.
  simpl in H.
  auto.
Qed.

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
  | choi p1 stop => stop
  | choi skip skip => skip
  | choi skip p2 => choi skip (smallstep_eval p2 e)
  | choi p1 skip => choi (smallstep_eval p1 e) skip
  | choi p1 p2 => if elemOfMenu e (menu p1) then choi (smallstep_eval p1 e) skip
                                            else if elemOfMenu e (menu p2) then choi skip (smallstep_eval p2 e)
                                                                           else choi p1 p2
  | seq skip skip => skip
  | seq skip p2 => seq skip (smallstep_eval p2 e)
  | seq stop p2 => stop
  | seq p1 p2 => seq (smallstep_eval p1 e) p2
  | par skip skip => skip
  | par skip p2 => par skip (smallstep_eval p2 e)
  | par p1 skip => par (smallstep_eval p1 e) skip
  | par p1 p2 => par (smallstep_eval p1 e) (smallstep_eval p2 e)
  end.

(* ver de probar este tipo de cosas ... *)
Lemma seqSkip : forall (p1 p2:Proc)(e:Event), OpSem p1 e p2 -> OpSem (seq skip p1) e p2.

Parameter ev:Event.
Eval compute in (match smallstep_eval stop ev with
                   stop => stop
                 | p => skip end).

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

Lemma sseval_pref_true : forall (e:Event)(p:Proc), smallstep_eval (pref e p) e = p.
Proof.
  intros e p.
  proc_unfold (smallstep_eval (pref e p) e).
  simpl.
  rewrite eq_event_ref.
  destruct p;auto.
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

Theorem sseval_correct : forall (e:Event)(p:Proc), OpSem p e (smallstep_eval p e).
Proof.
  cofix H.
  intros e p.
  case p.
  rewrite sseval_stop.
  constructor.
  rewrite sseval_skip.
  constructor.
  intros e1 p1.
  case_eq (


  simpl.
  compute.
  simpl.
Qed.