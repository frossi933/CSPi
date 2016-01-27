Require Import Coq.Arith.EqNat.

Definition Event : Set := nat.

(* Arbol de Trazas *)
CoInductive LTree (A:Set) :=
  tleaf : LTree A
| tcons : A -> LTree A -> LTree A
| tnode : LTree A -> LTree A -> LTree A.

Implicit Arguments tleaf [A].
Implicit Arguments tcons [A].
Implicit Arguments tnode [A].

Definition Traces : Set := LTree Event.

Definition trace_decompose (t:Traces) : Traces :=
  match t with
    tleaf => tleaf
  | tcons x xs => tcons x xs
  | tnode l r => tnode l r
  end.

Theorem trace_decompose_lemma : forall t:Traces, t = trace_decompose t.
Proof.
  intro t.
  case t;trivial.
Qed.

Ltac t_unfold term := rewrite (trace_decompose_lemma term);simpl.


(* Relacion de equivalencia entre arboles de traza *)
CoInductive bisimilar : Traces -> Traces -> Prop :=
  bis0 : bisimilar tleaf tleaf
| bis1 : forall (l l':Traces)(a:Event), bisimilar l l' -> bisimilar (tcons a l) (tcons a l')
| bis2 : forall (l l' r r':Traces), bisimilar l l' 
                                  -> bisimilar r r' 
                                  -> bisimilar (tnode l r) (tnode l' r').

Hint Resolve bis0 bis1 bis2 : ltree.

Notation "p ~=~ q" := (bisimilar p q) (at level 70, no associativity).

Theorem bis_refl : forall t, t ~=~ t.
Proof.
  cofix H.
  intro t.
  case t;constructor.
  apply (H l).
  apply (H l).
  apply (H l0).
Qed.


(* Expresiones del lenguaje CSP *)
CoInductive CspExp : Set := 
  stop : CspExp
| skip : CspExp
| pref : Event -> CspExp -> CspExp
| choi : CspExp -> CspExp -> CspExp
| seq : CspExp -> CspExp -> CspExp
| par :  CspExp -> CspExp -> CspExp.

Definition CE_decompose (e:CspExp) : CspExp :=
  match e with
    stop => stop
  | skip => skip
  | pref v p => pref v p
  | choi m p => choi m p
  | seq m p => seq m p
  | par m p => par m p
  end.

Theorem CE_decompose_lemma : forall e:CspExp, e = CE_decompose e.
Proof.
  intro e.
  case e;trivial.
Qed.

Ltac ce_unfold term := apply trans_equal with (1:=CE_decompose_lemma term).



CoFixpoint traces (e:CspExp) : Traces :=
  match e with
    stop => tleaf
  | skip => tleaf
  | pref v p => tcons v (traces p)
  | choi p q => tnode (traces p) (traces q)
  | seq p q => tleaf (* ?? *)
  | par p q => tnode (traces_aux p q) (traces_aux q p)
  end
with traces_aux (p q:CspExp) : Traces :=
  match p with
    stop => tleaf (* ver por casos del alpha *)
  | skip => tleaf
  | pref v r => tcons v (traces (par r q))
  | choi a b => tnode (traces (par a q)) (traces (par b q))
  | seq p q => tleaf (* ?? *)
  | par a b => tnode (traces (par a q)) (traces (par b q))
  end.

Theorem t1 : traces skip ~=~ traces stop.
Proof.
  t_unfold (traces skip).
  t_unfold (traces stop).
  constructor.
Qed.

(* axioma necesario (debido a la definicion de bisimilar) para probar la conmutatividad *)
Axiom tnode_conm : forall l r : Traces, tnode l r = tnode r l.

Theorem t2 : forall p q : CspExp, traces (choi p q) ~=~ traces (choi q p).
Proof.
  intros p q.
  rewrite (trace_decompose_lemma (traces (choi p q))).
  rewrite (trace_decompose_lemma (traces (choi q p))).
  simpl.
  rewrite (tnode_conm (traces p) (traces q)).
  constructor.
  exact (bis_refl (traces q)).
  exact (bis_refl (traces p)).
Qed.


(*
Ejemplo de teorema sin probar debido a la definicion de bisimilar...

Theorem t3 : forall p q r:CspExp, traces (choi (choi p q) r) ~=~ traces (choi p (choi q r)).
Proof.
  intros p q r.
  rewrite (trace_decompose_lemma (traces (choi (choi p q) r))).
  rewrite (trace_decompose_lemma (traces (choi p (choi q r)))).
  simpl.
  rewrite (trace_decompose_lemma (traces (choi p q))).
  rewrite (trace_decompose_lemma (traces (choi q r))).
  simpl.
  rewrite (tnode_conm (tnode (traces p) (traces q)) (traces r)).
*)


(* Parte de la Semantica Operacional para CSP *)
CoInductive OpSem : CspExp -> Event -> CspExp -> Prop :=
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

Theorem t4 : forall (p q r: CspExp)(e:Event), OpSem (choi p q) e r -> OpSem (choi q p) e r.
Proof.
  intros.
  inversion H.
  apply (sem3 q p r e);trivial.
  apply (sem2 q p r e);trivial.
Qed.

Theorem t5 : forall (p q r t:CspExp)(e:Event), OpSem (choi (choi p q) r) e t ->
                                               OpSem (choi p (choi q r)) e t.
Proof.
  intros p q r t e H.
  inversion H.
  inversion H4.
  apply (sem2 p (choi q r) t e);trivial.
  apply (sem3 p (choi q r) t e);trivial.
  apply (sem2 q r t e);trivial.
  apply (sem3 p (choi q r) t e);trivial.
  apply (sem3 q r t e);trivial.
Qed.

(*

-> esta bien la sem op? que dejo en OpSem y que no?
-> usar coind? podria hacerlo como en haskell? es decir, usando referencias a procesos.
-> como definir eval?
-> como definir funciones para asociarlas a eventos en coq?
-> como restringir cosas del lenguaje? por ejemplo a->p | a->q?
-> como manejar el no-determinismo?
-> esta bien mi idea de correctitud?

*)