Require Import List.
Require Import Coq.Arith.EqNat.

Definition IdEv : Set := nat.

Definition Input : Set := bool.
Parameter Output : Set.
Definition Pred : Set := nat -> Input.
Definition Act : Set := nat -> Output.

Inductive Event : Set :=
  Eps : Event
| EIn : IdEv -> Pred -> Event
| EOut : IdEv -> Act -> Event.

Definition EvSet := list Event.

Definition empty_set : EvSet := nil.


(* cual me conviene mas? dejo los dos? *)
Inductive Beq_event : Event -> Event -> Prop :=
  beq_eps : Beq_event Eps Eps
| beq_inin : forall (i:IdEv)(p1 p2:Pred), Beq_event (EIn i p1)(EIn i p2)
| beq_inout : forall (i:IdEv)(p:Pred)(a:Act), Beq_event (EIn i p)(EOut i a)
| beq_outout : forall (i:IdEv)(a1 a2:Act), Beq_event (EOut i a1)(EOut i a2)
| beq_outin : forall (i:IdEv)(a:Act)(p:Pred), Beq_event (EOut i a)(EIn i p).

Definition beq_event (e1 e2: Event) : bool := match e1, e2 with
  Eps, Eps => true
| EIn i1 p1 , EIn i2 p2 => beq_nat i1 i2
| EIn i1 p1 , EOut i2 a1 => beq_nat i1 i2
| EOut i1 a1 , EOut i2 a2 => beq_nat i1 i2
| EOut i1 a1 , EIn i2 p1 => beq_nat i1 i2
| _ , _ => false end.

Notation "a == b" := (Beq_event a b) (at level 70, no associativity).

Theorem eq_event_true : forall (e1 e2:Event), beq_event e1 e2 = true -> Beq_event e1 e2.
Proof.
  intros e1 e2 H.
  induction e1;induction e2.
  constructor.
  simpl in H.
  inversion H.
  simpl in H.
  inversion H.
  simpl in H.
  inversion H.
  simpl in H.
  rewrite (beq_nat_true i i0).
  constructor.
  assumption.
  inversion H.
  rewrite (beq_nat_true i i0).
  constructor;auto.
  assumption.
  inversion H.
  simpl in H.
  rewrite (beq_nat_true i i0).
  constructor;auto.
  assumption.
  inversion H.
  rewrite (beq_nat_true i i0).
  constructor.
  assumption.
Qed.

(* ????????  *)
Definition Aeq_dec: forall a b:Event, {a == b} + {a <> b}.


Inductive isMember: Event -> EvSet -> Prop :=
  sing: forall (e f:Event)(s:EvSet), Beq_event e f -> isMember e (cons f s)
| consmem: forall (e f:Event)(s:EvSet), isMember e s -> isMember e (cons f s).

Fixpoint isElem (e:Event)(s:EvSet) : bool := match s with
  nil => false
| cons x xs => if beq_event e x then true else isElem e xs
end.


Fixpoint set_add (e:Event) (x:EvSet) : EvSet :=
    match x with
    | nil => cons e nil
    | cons e1 x1 => if beq_event e e1 then cons e1 x1 else cons e1 (set_add e x1)
    end.

(* This function returns the greater event between the first parameter 
   and its equivalent inside the set. I consider "out events" greater than "in events".
   This helps me to select "out events" in the set intersection below because I will 
   need them in the execution process *)
Fixpoint getOutOf (e:Event)(s:EvSet) : Event :=
    match e with
    | EIn _ _ => match s with
                   nil => e
                 | f :: fs => if beq_event e f then f else getOutOf e fs
                 end
    | _ => e
    end.

Fixpoint set_inter (x:EvSet) : EvSet -> EvSet :=
    match x with
    | nil => fun y => nil
    | a1 :: x1 =>
        fun y =>
          if isElem a1 y then (getOutOf a1 y) :: set_inter x1 y else set_inter x1 y
    end.

Fixpoint set_union (x y:EvSet) : EvSet :=
    match y with
    | nil => x
    | a1 :: y1 => set_add a1 (set_union x y1)
    end.

Fixpoint set_diff (x y:EvSet) : EvSet :=
    match x with
    | nil => nil
    | a1 :: x1 =>
        if isElem a1 y then set_diff x1 y else set_add a1 (set_diff x1 y)
    end.