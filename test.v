Require Import Coq.Arith.EqNat.


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
  Eps : Event
| EIn : Id -> Pred -> Event
| EOut : Id -> Act -> Event.


Definition EvSet : Set := list Event.
Parameter execAct : Event -> Output.


Definition Name : Set := nat.



(* Expresiones del lenguaje CSP *)
Inductive Proc : Set := 
  stop : Proc
| skip : Proc
(*
| or: Proc -> Proc -> Proc 
| hide: Proc -> Event -> Proc *)
| pref : Event -> Proc -> Proc
| choi : Proc -> Proc -> Proc
| seq :  Proc -> Proc -> Proc
| par :  EvSet -> Proc -> Proc -> Proc
| id : Name -> Proc
| rec : Name -> Proc -> Proc.
(*
Definition Proc_decompose (e:Proc) : Proc :=
  match e with
    stop => stop
  | skip => skip
  | pref v p => pref v p
  | choi m p => choi m p
  | seq m p => seq m p
  | par s m p => par s m p
  | id n => id n
  | rec n p => rec n p
  end.

Lemma Proc_decompose_lemma : forall p:Proc, p = Proc_decompose p.
Proof.
  intro e.
  case e;trivial.
Qed.

Ltac proc_unfold term := apply trans_equal with (1:=Proc_decompose_lemma term).
*)

Definition beq_event (e1 e2: Event) : bool := match e1, e2 with
  EIn i1 p1 , EIn i2 p2 => beq_nat i1 i2
| EOut i1 a1 , EOut i2 a2 => beq_nat i1 i2
| _ , _ => false end.

Fixpoint isMember (e:Event)(s:EvSet) : Prop := match s with
  nil => False
| cons x xs => if beq_event e x then True else isMember e xs
end.

Fixpoint isElem (e:Event)(s:EvSet) : bool := match s with
  nil => false
| cons x xs => if beq_event e x then true else isElem e xs
end.

Fixpoint sust (p:Proc)(n:Name)(q:Proc) : Proc :=
match q with
  stop => stop
| skip => skip
| pref e r => pref e (sust p n r)
| choi p1 p2 => choi (sust p n p1) (sust p n p2)
| seq p1 p2 => seq (sust p n p1) (sust p n p2)
| par s p1 p2 => par s (sust p n p1) (sust p n p2)
| id m => if beq_nat n m then p else id m
| rec m r => rec m r (* ?? *)
end.

Inductive Op : Proc -> Event -> Proc -> Prop :=
  abort: Op stop Eps stop
| prefok: forall (e:Event)(p:Proc), Op (pref e p) e p
| choil: forall (e:Event)(p q r:Proc), Op p e q -> e <> Eps -> Op (choi p r) e q
| choir: forall (e:Event)(p q r:Proc), Op p e q -> e <> Eps -> Op (choi r p) e q
| choiepsl: forall (p q r:Proc), Op p Eps q -> Op (choi p r) Eps (choi q r)
| choiepsr: forall (p q r:Proc), Op p Eps q -> Op (choi r p) Eps (choi r q)
| skipl: forall (p:Proc), Op (choi skip p) Eps p
| skipr: forall (p:Proc), Op (choi p skip) Eps p
| sync: forall (e:Event)(p q r t:Proc)(s:EvSet), Op p e q -> Op r e t -> isMember e s -> Op (par s p r) e (par s q t)
| parl: forall (e:Event)(p q r:Proc)(s:EvSet), Op p e q -> ~(isMember e s) -> Op (par s p r) e (par s q r)
| parr: forall (e:Event)(p q r:Proc)(s:EvSet), Op p e q -> ~(isMember e s) -> Op (par s r p) e (par s r q)
| parskip: forall (s:EvSet), Op (par s skip skip) Eps skip
| seql: forall (e:Event)(p q r:Proc), Op p e q -> Op (seq p r) e (seq q r)
| seqskip: forall (p:Proc), Op (seq skip p) Eps p
| recursion: forall (p:Proc)(n:Name), Op (rec n p) Eps (sust (rec n p) n p)
| idref: forall (n:Name), Op (id n) Eps (id n). (* o "p" directamente *)
  
Fixpoint smallstep_eval (p:Proc) (e:Event) : option Proc :=
  match p, e with 
    stop, Eps => Some stop
  | stop, v => None
  | skip, v => None
  | pref v r, v' => if beq_event v v' then Some r else None
  | choi p1 p2, Eps => match smallstep_eval p1 Eps, smallstep_eval p2 Eps with
                         None, None => None
                       | Some p3, None => Some (choi p3 p2)
                       | None, Some p4 => Some (choi p1 p4)
                       | Some p3, Some p4 => Some (choi p3 p2) (* hacerlo random *)
                       end
  | choi p1 p2, v => match smallstep_eval p1 v, smallstep_eval p2 v with
                       None, None => None
                     | Some p3, None => Some p3
                     | None, Some p4 => Some p4
                     | Some p3, Some p4 => Some p3 (* hacerlo random *)
                     end
  | seq skip p, Eps => Some p
  | seq p1 p2, v => match smallstep_eval p1 v with
                      Some p3 => Some (seq p3 p2)
                    | None => None
                    end
  | par s skip skip, Eps => Some skip
  | par s p1 p2, v => if isElem v s then match smallstep_eval p1 v, smallstep_eval p2 v with
                                           Some p3, Some p4 => Some (par s p3 p4)
                                         | _ , _ => None
                                         end
                                    else match smallstep_eval p1 v, smallstep_eval p2 v with
                                           Some p3, None => Some (par s p3 p2)
                                         | None, Some p4 => Some (par s p1 p4)
                                         | _, _ => None
                                         end
  | rec n p, Eps => (* chequear n *)Some (sust (rec n p) n p)
  | id n, Eps => Some (id n) (* ?? *)
  | p , v => None
  end.

Parameter pred:Pred.
Definition e:Event := EIn 1 pred.

Definition p : Proc := rec 0 (pref Eps (id 0)).

Eval compute in (smallstep_eval p Eps).

Inductive NullProc : Proc -> Prop :=
  nullstop : NullProc stop
| nullchoil : forall p q:Proc, NullProc q -> NullProc (choi q p)
| nullchoir : forall p q:Proc, NullProc q -> NullProc (choi p q)
| nullseq : forall p q:Proc, NullProc q -> NullProc (seq q p)
| nullseq2 : forall p:Proc, NullProc p -> NullProc (seq skip p)
| nullparl : forall (p q:Proc)(s:EvSet), NullProc q -> NullProc (par s q p)
| nullparr : forall (p q:Proc)(s:EvSet), NullProc q -> NullProc (par s p q).

Lemma sseval_nullproc: forall (e:Event)(p:Proc), NullProc p -> smallstep_eval p e = None.
Proof.
  intros e p H.
  induction H;intros;simpl.
Theorem sseval_correct: forall (e:Event)(p q:Proc), smallstep_eval p e = Some q -> Op p e q.
Proof.
  intros e p q H.
  induction p;induction e;intros;simpl.
  compute in H. inversion H. constructor.
  compute in H;inversion H.
  compute in H;inversion H.
  compute in H;inversion H.
  compute in H;inversion H.
  compute in H;inversion H.
  compute in H.
  inversion H.

  compute in H.
  case_eq e;intro H1.
  rewrite H1 in H;auto.
  constructor.