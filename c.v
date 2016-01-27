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

Theorem Proc_decompose_lemma : forall e:Proc, e = Proc_decompose e.
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

CoInductive isTrace : Trace -> Proc -> Prop :=
  nilt : forall p:Proc, isTrace nil p (* ?? *)
| preft : forall (t:Trace)(p:Proc)(e:Event), isTrace t p -> isTrace ((execAct e)::t) (pref e p)
| choit1 : forall (t1 t2:Trace)(p1 p2:Proc), isTrace t1 p1 -> isTrace t2 p2 -> isTrace t1 (choi p1 p2)
| choit2 : forall (t1 t2:Trace)(p1 p2:Proc), isTrace t1 p1 -> isTrace t2 p2 -> isTrace t2 (choi p1 p2)
| seqt: forall (t1 t2:Trace)(p1 p2:Proc), isTrace t1 p1 -> succTerm p1 -> isTrace t2 p2 -> isTrace (concatt t1 t2) (seq p1 p2).


(* Parte de la Semantica Operacional para CSP *)
CoInductive OpSem : Proc -> Event -> Proc -> Prop :=
  sem1: forall (p:CspExp)(e:Event), OpSem (pref e p) e p
| sem2: forall (p q r:CspExp)(e:Event), OpSem p e r -> OpSem (choi p q) e r
| sem3: forall (p q r:CspExp)(e:Event), OpSem q e r -> OpSem (choi p q) e r
| sem4: forall (p q r:CspExp)(e:Event), OpSem p e r -> OpSem (par p q) e (par r q)
| sem5: forall (p q r:CspExp)(e:Event), OpSem q e r -> OpSem (par p q) e (par p r)
| sem6: forall (p q r:CspExp)(e:Event), OpSem p e r -> OpSem (seq p q) e (seq r q)
| sem7: forall (p r:CspExp)(e:Event), OpSem p e r -> OpSem (seq skip p) e r (* ?? *)
| sem7a: forall (p r:CspExp)(e:Event), OpSem (seq skip p) e p (* ?? *)
| sem8: forall (p :CspExp)(e:Event), OpSem (seq stop p) e stop
| sem9: forall (e:Event), OpSem stop e stop
| sem10:forall (e:Event), OpSem skip e skip.

Parameter e : Event.
CoFixpoint a:CspExp := pref e a.

CoFixpoint choi_eval (a b:CspExp) (e:Event) : CspExp :=
  match a, b with 
    skip, skip => skip (* no deberia ocurrir, ? *)
  | skip, pref v r => if beq_nat v e then r else pref v r
  | skip, stop => skip
  | skip, choi aa bb => choi skip (choi_eval aa bb e)
  | skip, seq aa bb => choi skip (seq_eval aa bb e)
  | skip, par aa bb => choi skip (par_eval aa bb e)
  | pref v r, bb => if beq_nat v e then r else pref v r
  | aa, bb => bb
  end
with seq_eval (pp qq:CspExp)(e:Event) : CspExp :=
  match pp with
    skip => match qq with 
              skip => skip
            | stop => stop
            | pref v r => if beq_nat v e then r else pref v r
            | ppp => skip
            end
  | stop => stop
  | pref v r => if beq_nat v e then (seq r qq) else match qq with 
                                                      skip => pp
                                                    | stop => pp
                                                    | qqq => (seq pp qqq)
                                                    end
  | p => skip
  end.


CoFixpoint smallstep_eval (p:CspExp) (e:Event) : CspExp :=
  match p with 
    stop => stop
  | skip => skip
  | pref v r => if beq_nat v e then r else pref v r
  | choi a b => choi_eval a b e 
  | seq a b => seq_eval a b e
  | par a b => par (smallstep_eval a e) (smallstep_eval b e)
  end.