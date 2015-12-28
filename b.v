Definition Event : Set := nat.

CoInductive LTree (A:Set) :=
  tleaf : LTree A
| tcons : A -> LTree A -> LTree A
| tnode : LTree A -> LTree A -> LTree A.

Implicit Arguments tleaf [A].
Implicit Arguments tcons [A].
Implicit Arguments tnode [A].

Definition Traces : Set := LTree Event.

CoInductive bisimilar : Traces -> Traces -> Prop :=
  bis0 : bisimilar tleaf tleaf
| bis1 : forall (l l':Traces)(a:Event), bisimilar l l' -> bisimilar (tcons a l) (tcons a l')
| bis2 : forall (l l' r r':Traces), bisimilar l l' 
                                  -> bisimilar r r' 
                                  -> bisimilar (tnode l r) (tnode l' r').

Hint Resolve bis0 bis1 bis2 : ltree. (* revisar *)

Notation "p ~=~ q" := (bisimilar p q) (at level 70, no associativity).

CoInductive CspExp : Set := 
  stop : CspExp
| skip : CspExp
| pref : Event -> CspExp -> CspExp
| choi : CspExp -> CspExp -> CspExp
| par :  CspExp -> CspExp -> CspExp.

Definition CE_decompose (e:CspExp) : CspExp :=
  match e with
    stop => stop
  | skip => skip
  | pref v p => pref v p
  | choi m p => choi m p
  | par m p => par m p
  end.


CoFixpoint traces (e:CspExp) : Traces :=
  match e with
    stop => tleaf
  | skip => tleaf
  | pref v p => tcons v (traces p)
  | choi p q => tnode (traces p) (traces q)
  | par p q => tnode (traces_aux p q) (traces_aux q p)
  end
with traces_aux (p q:CspExp) : Traces :=
  match p with
    stop => tleaf (* ver por casos del alpha *)
  | skip => tleaf
  | pref v r => tcons v (traces (par r q))
  | choi a b => tnode (traces (par a q)) (traces (par b q))
  | par a b => tnode (traces (par a q)) (traces (par b q))
  end.

Theorem t1 : forall p q : CspExp, traces (choi p q) ~=~ traces (choi q p).
Proof.
  intros.
  

Qed.

