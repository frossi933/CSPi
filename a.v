Definition Label := nat.
Inductive Func : Set := outf | inf.
Definition Event : Set := Label -> Func.
Parameter tick : Event.

Definition Alpha : Type := list Event.



Inductive CspExp : Set := 
  stop : CspExp
| skip : CspExp
| pref : Event -> CspExp -> CspExp
| choi : CspExp -> CspExp -> CspExp
| par :  CspExp -> CspExp -> CspExp.

Print list.

Fixpoint alpha (e:CspExp) : Alpha := match e with
            stop => nil
          | skip => cons tick nil
          | pref v e' => cons v (alpha e')
          | choi e1 e2 => app (alpha e1) (alpha e2)
          | par e1 e2 => app (alpha e1) (alpha e2) end.

CoInductive CspExpCo : Set :=
  costop : CspExpCo
| coskip : CspExpCo
| copref : Event -> CspExpCo -> CspExpCo
| cochoi : CspExpCo -> CspExpCo -> CspExpCo
| copar :  CspExpCo -> CspExpCo -> CspExpCo.

(* its application on the result of co-recursive functions provokes an unfolding of these co-rec functions *)
Definition CEC_decompose (e:CspExpCo) : CspExpCo :=
  match e with
    costop => costop
  | coskip => coskip
  | copref v p => copref v p
  | cochoi m p => cochoi m p
  | copar m p => copar m p
  end.

CoInductive AlphaCo : Set := Anil : AlphaCo 
                           | Acons : Event -> AlphaCo -> AlphaCo.

Definition Alpha_decompose (a:AlphaCo) : AlphaCo :=
  match a with
    Anil => Anil
  | Acons v a' => Acons v a'
  end.

CoFixpoint Alpha_decompose_n (n:nat)(a:AlphaCo) : AlphaCo :=
  match n with
    0   => a
  | S m => match a with
             Anil => Anil
           | Acons v a' => Acons v (Alpha_decompose_n m a')
           end
  end.

(* - The result type must be a co-inductive type
   - all recursive calls must occur inside one of the arguments of a constructor of a co-ind type
     (this derivates from lazy computations)
*)
CoFixpoint concat (a1 a2:AlphaCo) : AlphaCo := match a1 with
            Anil => a2
          | Acons e a1' => match a2 with
                        Anil => a1
                      | Acons e' a2' => Acons e (Acons e' (concat a1' a2')) end end.

Theorem CEC_decompose_lemma : forall (e:CspExpCo), e = CEC_decompose e.
Proof.
  intro e.
  case e;auto.
Qed.

Ltac CEC_unfold term := apply trans_equal with (1:= CEC_decompose_lemma term).

CoInductive LList (A:Set) : Set :=
  Lnil : LList A
| Lcons : A -> LList A -> LList A.

CoInductive Traces : CspExpCo -> LList Event -> Prop :=
  tstop : Traces costop (Lnil Event)
| tskip : Traces coskip (Lnil Event)
| tpref : forall (p:CspExpCo)(a:LList Event)(e:Event), Traces p a -> Traces (copref e p) (Lcons Event e a)
| tchoi1 : forall (p q:CspExpCo)(a b:LList Event), Traces p a -> Traces q b -> Traces (cochoi p q) a
| tchoi2 : forall (p q:CspExpCo)(a b:LList Event), Traces p a -> Traces q b -> Traces (cochoi p q) b
| tpar : forall (p q:CspExpCo)(a b:LList Event), Traces p a -> Traces q b -> Traces (cochoi p q) a. (*cambiar*)


Check (Traces (cochoi (copref 1 costop) (copref 2 coskip)) (Lcons 1 (Lcons 2 Lnil))).

CoFixpoint alphaco (e:CspExpCo) : AlphaCo := match e with
            costop => Anil
          | coskip => Acons tick Anil
          | copref v e' => Acons v (alphaco e')
          | cochoi e1 e2 => 
          | copar e1 e2 => concat (alphaco e1) (alphaco e2) end.

Notation "e -> P" := (pref e P).
