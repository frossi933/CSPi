Require Import Coq.Arith.EqNat.
Require Import Coq.Lists.List.


Definition Id : Set := nat.
Definition Input : Set := bool.
Parameter Output : Set.
(*
Definition Output : Set := nat.
Parameter Pred : nat -> Input.
Parameter Act : nat -> Output.
*)
Definition Pred : Set := nat -> Input.
Definition Act : Set := nat -> Output.


Inductive Event : Set :=
  EIn : Id -> Pred -> Event
| EOut : Id -> Act -> Event.

Definition EvSet : Set := list Event.
Parameter execAct : Event -> Output.

(*
Parameter beq_event : Event -> Event -> bool.
Axiom event_ref : forall e:Event, beq_event e e = true.
*)

(* cual me conviene mas? dejo los dos? *)
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

Theorem beq_event_true : forall (e1 e2:Event), Beq_event e1 e2 -> beq_event e1 e2 = true.
Proof.
  intros e1 e2 H.
  inversion H; simpl;rewrite H0;rewrite (beq_nat_refl i2);auto.
Qed.

Axiom classic_beq_event : forall (e1 e2:Event), Beq_event e1 e2 \/ ~(Beq_event e1 e2).

Fixpoint isMember (e:Event)(s:EvSet) : Prop := match s with
  nil => False
| cons x xs => if beq_event e x then True else isMember e xs
end.
(* ?? *)
Fixpoint isElem (e:Event)(s:EvSet) : bool := match s with
  nil => false
| cons x xs => if beq_event e x then true else isElem e xs
end.