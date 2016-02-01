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
| sem8: forall (p :Proc)(e:Event), OpSem (seq stop p) e stop
| sem9: forall (e:Event), OpSem stop e stop
| sem10:forall (e:Event), OpSem skip e skip.

Parameter e : Event.
CoFixpoint a:Proc := pref e a.

Require Import Coq.Arith.EqNat.

Parameter beq_event : Event -> Event -> bool.

(*CoFixpoint choi_eval (p1 p2:Proc) (e:Event) : Proc :=


  | skip, pref v r => if beq_event v e then r else pref v r
  | skip, stop => skip
  | skip, choi aa bb => choi skip (choi_eval aa bb e)
  | skip, seq aa bb => choi skip (seq_eval aa bb e)
  | skip, par aa bb => choi skip (par_eval aa bb e)

  | pref v r, bb => if beq_event v e then r else pref v r
  | aa, bb => bb
  end

CoFixpoint seq_eval (pp qq:Proc)(e:Event) : Proc :=
  match pp with
    skip => match qq with 
              skip => skip
            | stop => stop
            | pref v r => if beq_event v e then r else pref v r
            | ppp => skip
            end
  | stop => stop
  | pref v r => if beq_event v e then (seq r qq) else match qq with 
                                                      skip => pp
                                                    | stop => pp
                                                    | qqq => (seq pp qqq)
                                                    end
  | p => skip
  end
with par_eval (pp qq:Proc)(e:Event) : Proc :=
  match pp, qq with
    skip, skip => skip
  | w , i => skip
  end
with
*)


CoInductive Menu : Set :=
  none : Menu
| some : Event -> Menu -> Menu.

CoFixpoint concatm (m1 m2:Menu) : Menu :=
  match m1 with
    none => m2
  | some e m1' => some e (concatm m1' m2)
  end.

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

CoFixpoint smallstep_eval (p:Proc) (e:Event) : Proc :=
  match p with 
    stop => stop
  | skip => skip
  | pref v r => if beq_event v e then r else pref v r
  | choi p1 p2 => match p1, p2 with 
                  stop, p2' => stop
                | p1', stop => stop
                | skip, skip => skip (* no deberia ocurrir, ? *)
                | skip, p2' => choi skip (smallstep_eval p2' e)
                | p1', skip => choi (smallstep_eval p1' e) skip
                | pref v1 r1 , p2' => if beq_event v1 e then r1 
                                      else_event v2 e then r2 else p
                | pref v1 r1, choi pp1 pp2 => if beq_event v1 e then r1 else
                                              
                | p1', p2' => skip
                end
  | seq p1 p2 => match p1, p2 with
                   skip, skip => skip
                 | skip, p2' => seq skip (smallstep_eval p2' e)
                 | stop, p2' => stop
                 |  => if beq_event v e then (seq r p2) else match p2 with 
                                                      skip => p1
                                                    | stop => p1
                                                    | qqq => (seq p1 qqq)
                                                    end
                 | p => skip
                 end
  | par a b => par (smallstep_eval a e) (smallstep_eval b e)
  end.