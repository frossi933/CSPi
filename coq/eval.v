Require Import Coq.Arith.EqNat.

Load event.

Definition Menu := EvSet.
Definition Name : Set := nat.

(* CSP expressions *)
Inductive Proc : Type := 
  stop : Proc
| skip : Proc
(*
| or: Proc -> Proc -> Proc 
| hide: Proc -> Event -> Proc *)
| pref : Event -> Proc -> Proc
| choi : Proc -> Proc -> Proc
| seq :  Proc -> Proc -> Proc
| par :  EvSet -> Proc -> Proc -> Proc
| id : Name -> Proc.

(* Environment of Process Definitions *)
Parameter Env: Name -> Proc.

(* Operational Semantics of CSP *)
Inductive Op : Proc -> Event -> Proc -> Prop :=
| prefok:    forall (e0 e:Event)(p:Proc), Beq_event e0 e -> Op (pref e0 p) e p
| choil:     forall (e:Event)(p q r:Proc), Op p e q -> e <> Eps -> Op (choi p r) e q
| choir:     forall (e:Event)(p q r:Proc), Op p e q -> e <> Eps -> Op (choi r p) e q
| choiepsl:  forall (p q r:Proc), Op p Eps q -> Op (choi p r) Eps (choi q r)
| choiepsr:  forall (p q r:Proc), Op p Eps q -> Op (choi r p) Eps (choi r q)
| skipl:     forall (p:Proc), Op (choi skip p) Eps p
| skipr:     forall (p:Proc), Op (choi p skip) Eps p
| choistop:  Op (choi stop stop) Eps stop
| sync:      forall (e:Event)(p q r t:Proc)(s:EvSet), Op p e q -> Op r e t -> isMember e s -> Op (par s p r) e (par s q t)
| parl:      forall (e:Event)(p q r:Proc)(s:EvSet), Op p e q -> ~(isMember e s) -> Op (par s p r) e (par s q r)
| parr:      forall (e:Event)(p q r:Proc)(s:EvSet), Op p e q -> ~(isMember e s) -> Op (par s r p) e (par s r q)
| parskip:   forall (s:EvSet), Op (par s skip skip) Eps skip
| seql:      forall (e:Event)(p q r:Proc), Op p e q -> Op (seq p r) e (seq q r)
| seqskip:   forall (p:Proc), Op (seq skip p) Eps p
| seqstop:   forall (p:Proc), Op (seq stop p) Eps stop (* ?? *)
| idref:     forall (n:Name), Op (id n) Eps (Env n).
  
(* Small-step evaluator of one process through events *)
Fixpoint smallstep_eval (p:Proc) (e:Event) : option Proc :=
  match p, e with 
  | stop, v => None
  | skip, v => None
  | pref v r, v' => if beq_event v v' then Some r else None
  | choi stop stop, Eps => Some stop
  | choi skip p2, Eps => Some p2
  | choi p1 skip, Eps => Some p1
  | choi p1 p2, Eps => match smallstep_eval p1 Eps, smallstep_eval p2 Eps with
                         None, None => None
                       | Some p3, None => Some (choi p3 p2)
                       | None, Some p4 => Some (choi p1 p4)
                       | Some p3, Some p4 => Some (choi p3 p2) (* TODO: random *)
                       end
  | choi p1 p2, v => match smallstep_eval p1 v, smallstep_eval p2 v with
                       None, None => None
                     | Some p3, None => Some p3
                     | None, Some p4 => Some p4
                     | Some p3, Some p4 => Some p3 (* TODO: random *)
                     end
  | seq skip p, Eps => Some p
  | seq stop p, Eps => Some stop   (* ?? *)
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
  | id n, Eps => Some (Env n) (* ?? *)
  | p , v => None
  end.


(* Little Example *)
Parameter pred:Pred.
Definition ee:Event := EIn 1 pred.
Definition pp : Proc := (pref Eps (id 0)).
Eval compute in (smallstep_eval pp Eps).



(* Some tactics to prove sseval_correct theorem *)
Ltac choi_lr := 
match goal with
  IHp1:(forall _ : Proc, _ ), H4: smallstep_eval ?X1 ?E = Some ?X3 |- Op (choi ?X1 ?X2) ?E ?X3 => apply choil; [apply IHp1;auto | discriminate ]
| IHp2:(forall _ : Proc, _ ), H3: smallstep_eval ?X2 ?E = Some ?X3 |- Op (choi ?X1 ?X2) ?E ?X3 => apply choir; [apply IHp2;auto | discriminate ]
end.

Ltac par_tac := match goal with
  IHp1:(forall _ : Proc, _ ) ,
  IHp2:(forall _ : Proc, _ ) ,
  h1: smallstep_eval ?X1 ?E = Some ?X3,
  h2: smallstep_eval ?X2 ?E = Some ?X4 |- Op (par ?S ?X1 ?X2) ?E (par ?S ?X3 ?X4) => apply sync;
                                                                                     [exact (IHp1 X3 h1) | exact (IHp2 X4 h2) | apply (iselemmember E S);auto]
| IHp1:(forall _ : Proc, _ ) ,
  he: isElem ?E ?S = false,
  h1: smallstep_eval ?X1 ?E = Some ?X3 |- Op (par ?S ?X1 ?X2) ?E (par ?S ?X3 ?X2) => apply parl;
                                                                                     [apply (IHp1 X3 h1) | exact (not_mem E S he)]
| IHp2:(forall _ : Proc, _ ) ,
  he: isElem ?E ?S = false,
  h2: smallstep_eval ?X2 ?E = Some ?X4 |- Op (par ?S ?X1 ?X2) ?E (par ?S ?X1 ?X4) => apply parr;
                                                                                     [apply (IHp2 X4 h2) | exact (not_mem E S he)]
end.

Ltac choi_cases := 
match goal with
  h1:?X1 = stop ,h2: ?X2=stop |- Op (choi ?X1 ?X2) Eps stop => rewrite h1, h2;apply choistop
| h1:?X1 = skip               |- Op (choi ?X1 ?X2) Eps ?X2 => rewrite h1;apply skipl
| h1:?X2 = skip               |- Op (choi ?X1 ?X2) Eps ?X1 => rewrite h1;apply skipr
| _ => idtac
end.

Ltac choi_cases2 p1 p2 := 
case_eq (smallstep_eval p1 Eps);case_eq (smallstep_eval p2 Eps);intros;
match goal with
  H: _ = Some ?Q, IHp1:(forall _ : Proc, _ ), h1:(smallstep_eval p1 Eps)=Some ?X1, h2:(smallstep_eval p2 Eps)=_ |- Op (choi p1 p2) Eps ?Q => 
          rewrite h1 in *;rewrite h2 in *;inversion H;apply choiepsl;apply IHp1;trivial
| H:_ = Some ?Q, IHp2:(forall _ : Proc, _ ), h1:(smallstep_eval p1 Eps)=None, h2:(smallstep_eval p2 Eps)=Some ?X1 |- Op (choi p1 p2) Eps ?Q => 
          rewrite h1 in *;rewrite h2 in *;inversion H;apply choiepsr;apply IHp2;trivial
| H:_ = Some ?Q, h1:(smallstep_eval p1 Eps)=None, h2:(smallstep_eval p2 Eps)=None |- Op (choi p1 p2) Eps ?Q => 
            rewrite h1 in *;rewrite h2 in *;inversion H
end.


Theorem sseval_correct: forall (e:Event)(p q:Proc), smallstep_eval p e = Some q -> Op p e q.
Proof.
  intros e p.
  induction p;intros. simpl.
  case_eq e;intros.
  rewrite H0 in H.
  inversion H;constructor.
  rewrite H0 in H.
  inversion H;constructor.
  rewrite H0 in H.
  inversion H;constructor.
  compute in H;inversion H.
  case_eq (beq_event e0 e);intro H0; unfold smallstep_eval in H;rewrite H0 in H.
   inversion H.
   rewrite <- H2.
   apply (prefok e0 e p (eq_event_true e0 e H0)).
   inversion H.

  (* CHOICE *)
  case_eq e.
  intro H0;rewrite H0 in *.
  simpl in H.
  case_eq p1;intros;rewrite H1 in H at 1;simpl in H;rewrite <- H1;case_eq p2;intros;try(rewrite H2 in H at 1;simpl in H);rewrite <- H2;inversion H;
  choi_cases;
  choi_cases2 p1 p2.

  intros i pr H0.
  simpl in H.
  rewrite H0 in *. 
  case_eq p1;intros;rewrite H1 in H at 1;simpl in H;rewrite <- H1;case_eq p2;intros;rewrite <- H2;try(rewrite H2 in H at 1;simpl in H);
  case_eq (smallstep_eval p1 (EIn i pr));case_eq (smallstep_eval p2 (EIn i pr));intros;rewrite H3 in *;rewrite H4 in *;inversion H;rewrite <- H6;
  try(choi_lr).

  intros i a H0.
  simpl in H.
  rewrite H0 in *.
  case_eq p1;intros;rewrite H1 in H at 1;simpl in H;rewrite <- H1;case_eq p2;intros;rewrite <- H2;try(rewrite H2 in H at 1;simpl in H);
  case_eq (smallstep_eval p1 (EOut i a));case_eq (smallstep_eval p2 (EOut i a));intros;rewrite H3 in *;rewrite H4 in *;inversion H;rewrite <- H6;
  try(choi_lr).

  (* SEQ *)
  simpl in H.
  case_eq p1;intros;rewrite H0 in *;
  (* <> skip *) try (rewrite <- H0 in H;case_eq (smallstep_eval p1 e);intros;rewrite H1 in *;
                     inversion H;constructor;apply IHp1;rewrite H0 in H1;trivial;
                     case_eq (smallstep_eval (pref e0 p0) e);intros;rewrite H1 in *;
                     inversion H;constructor;apply IHp1;trivial).
  (* = skip *)
  case_eq e;intros;rewrite H1 in *;inversion H;constructor.
  case_eq e;intros;rewrite H1 in *;inversion H.
  constructor.

  (* PAR *)
  simpl in H.
  case_eq p1;[ intros H0 | intros H0 | intros e1 p0 H0 | intros p0 p3 H0 | intros p0 p3 H0 | intros e1 p0 p3 H0 | intros n H0];
  rewrite H0 in H;rewrite <- H0 in H;rewrite <- H0;case_eq p2;intros;rewrite H1 in H;rewrite <- H1 in H;rewrite <- H1;
  case_eq (isElem e e0);intro H2;rewrite H2 in H;case_eq(smallstep_eval p1 e);intros;rewrite H3 in H;
  inversion H;
  case_eq (smallstep_eval p2 e);intros;try(rewrite H4 in H);inversion H;try(par_tac).
  (* skip,skip*)
  rewrite H0 in H3;inversion H3.
  rewrite H0 in H3;inversion H3.
  rewrite H1 in H4;inversion H4.
  destruct e;simpl;inversion H5.
  rewrite H1;rewrite H0 in H8;rewrite <- H8.
  constructor.
  rewrite H1 in H4;inversion H4.
  rewrite H0 in H3;inversion H3.
  rewrite H1 in H4;inversion H4.
  rewrite H0,H1 in *.
  destruct e;simpl;inversion H5.
  constructor.

  (* ID *)  
  simpl in H.
  destruct e;simpl;inversion H.
  constructor.
Qed.
  
(* Trace definition. It represents the result of one execution from the user's point of view *)
CoInductive Trace : Set :=
  nilt : Trace
| const : Output -> Trace -> Trace.

Notation "e :: t" := (const e t).

Definition trace_decompose (t:Trace) : Trace :=
match t with
  nilt => nilt 
| const o t' => const o t'
end.

Theorem trace_dec_lemma : forall (t:Trace), t = trace_decompose t.
Proof.
  intros.
  case t;trivial.
Qed.
Check trans_equal.
Check trace_dec_lemma.

Ltac trace_unfold term := rewrite (trace_dec_lemma term).

Parameter execAct : Event -> Output.
Parameter test : Pred -> bool.
Parameter tick : Output.
Parameter empty : Output.
Parameter rand : unit -> bool.

Axiom exec_eps : execAct Eps = empty.
Axiom exec_ein : forall (i:IdEv)(pr:Pred), execAct (EIn i pr) = empty.
Axiom exec_eout : forall (i:IdEv)(f:Act), execAct (EOut i f) <> empty.

CoInductive isTrace : Trace -> Proc -> Prop :=
  skip_tick : isTrace (tick :: nilt) skip
| empt:       forall (t:Trace)(p1 p2:Proc), Op p1 Eps p2 -> 
                                        isTrace t p2 -> 
                                        isTrace (empty :: t) p1
| nil_is_t: forall p:Proc, isTrace nilt p
| preft :   forall (t:Trace)(p:Proc)(e1 e2:Event), Beq_event e2 e1 -> isTrace t p -> isTrace ((execAct e1)::t) (pref e2 p)
| choitl :  forall (t:Trace)(p1 p2:Proc), isTrace t p1 -> isTrace t (choi p1 p2)
| choitr :  forall (t:Trace)(p1 p2:Proc), isTrace t p2 -> isTrace t (choi p1 p2)
| seqt:     forall (t:Trace)(p1 p2 p3:Proc)(e:Event), isTrace t (seq p1 p2) -> Op p3 e p1 -> isTrace ((execAct e) :: t) (seq p3 p2)
| parsynct: forall (t:Trace)(e:Event)(s:EvSet)(p1 p2 p3 p4:Proc),
                   isMember e s -> Op p3 e p1 -> Op p4 e p2 -> isTrace t (par s p1 p2) -> isTrace (execAct e :: t) (par s p3 p4)
| parlt:    forall (t:Trace)(e:Event)(s:EvSet)(p1 p2 p3:Proc), ~isMember e s -> Op p3 e p1 -> isTrace t (par s p1 p2) -> isTrace (execAct e :: t) (par s p3 p2)
| parrt:    forall (t:Trace)(e:Event)(s:EvSet)(p1 p2 p3:Proc), ~isMember e s -> Op p3 e p2 -> isTrace t (par s p1 p2) -> isTrace (execAct e :: t) (par s p1 p3).


Fixpoint menu (p:Proc) : Menu := match p with
   stop => empty_set
 | skip => empty_set
 | pref (EIn i pred) q => if test pred then set_add (EIn i pred) empty_set else empty_set
 | pref e q => set_add e empty_set
 | choi skip q => set_add Eps (menu q)
 | choi q skip => set_add Eps (menu q)
 | choi stop stop => set_add Eps empty_set
 | choi r q => set_union (menu r) (menu q)
 | seq skip q => set_add Eps empty_set
 | seq stop q => set_add Eps empty_set (*??*)
 | seq r q => menu r
 | par s skip skip => set_add Eps empty_set
 | par s r q => set_union (set_inter (set_inter (menu r) (menu q)) s) 
                          (set_union (set_diff (menu r) s) (set_diff (menu q) s))
 | id n => set_add Eps empty_set
end.


Fixpoint choose (m:Menu) : option Event := match m with
  nil => None
| cons e es => match es with
                 nil => Some e
               | cons f fs => if rand tt then Some e
                                         else choose es
               end
end.


Theorem menu_correct : forall (p:Proc)(e:Event), isMember e (menu p) -> exists q:Proc, smallstep_eval p e = Some q.
(* TODO *)

Theorem menu_correct2: forall (p:Proc)(e:Event), choose (menu p) = Some e -> 
                                                exists q:Proc, smallstep_eval p e = Some q.
(* TODO *)


CoFixpoint eval (p:Proc) : Trace := match p with
  stop => nilt
| skip => tick :: nilt
| q => match choose (menu q) with
         None => nilt (* esperar que se active alguno o morir *)
       | Some e => match smallstep_eval q e with
                     None => nilt (* no deberia entrar nunca aca *)
                   | Some r => execAct e :: (eval r)
                   end
       end
end.


(* Assuming "isTrace t p" and using preft clause from isTrace definition, it is possible to prove that:
     isTrace (execAct (EOut i f) :: t) (pref (EIn i pr) p)
   But that is not correct. Thus, the following lemma proves it is impossible to obtain that
   as a result of eval function.
*)
Lemma l1: forall (p:Proc)(i:IdEv)(pr:Pred)(f:Act), eval (pref (EIn i pr) p) <> (execAct (EOut i f) :: (eval p)).
Proof.
  intros p i pr f.
  trace_unfold (eval (pref (EIn i pr) p));simpl.
  destruct (test pr);simpl.
  rewrite <- (beq_nat_refl i).  
  unfold not;intro H.
  inversion H.
  rewrite (exec_ein i pr) in H1.
  elim (exec_eout i f);auto.
  unfold not;intro H.
  inversion H.
Qed.  


(* If there is a sequence of processes {Pi}i~N with Po =p1 and Op Pi Eps Pi+1 
then I say that p1 has the possibility to diverge. *)
CoInductive Diverge : Proc -> Prop :=
  lockdiv: forall n:Name, Env n = (id n) -> Diverge (id n)
| loopdiv: forall n:Name, Diverge (Env n) -> Diverge (id n)
| prefdiv: forall (p:Proc)(e:Event), Diverge p -> Diverge (pref e p) (* ??*)
| choildiv: forall p q:Proc, Diverge p -> Diverge (choi p q)
| choirdiv: forall p q:Proc, Diverge p -> Diverge (choi q p)
| seqldiv: forall p q:Proc, Diverge p -> Diverge (seq p q)
| seqdiv: forall p q:Proc, Diverge q -> Diverge (seq p q)
| parldiv: forall (p q:Proc)(s:EvSet), Diverge p -> Diverge (par s p q)
| parrdiv: forall (p q:Proc)(s:EvSet), Diverge p -> Diverge (par s q p).

Axiom eval_op: forall (p q:Proc)(e:Event), Op p e q -> eval p = execAct e :: (eval q).
(* Is this correct? *)

Lemma div_pred : forall (p q:Proc)(e:Event), Op p e q -> Diverge q -> Diverge p.
Proof.
  induction p;intros;simpl;inversion H.
  constructor;trivial.
  apply choildiv;exact (IHp1 q e H3 H0).
  apply choirdiv;exact (IHp2 q e H3 H0).
  rewrite <- H4 in H0.
  inversion H0.
   apply choildiv;exact (IHp1 q0 Eps H5 H7).
   apply choirdiv;trivial.
  rewrite <- H4 in H0.
  inversion H0.
   apply choildiv;trivial.
   apply choirdiv;exact (IHp2 q0 Eps H5 H7).
  apply choirdiv;trivial.
  apply choildiv;trivial.
  rewrite <- H4 in H0;inversion H0.
  rewrite <- H4 in H0;inversion H0.
  apply seqldiv.
   exact (IHp1 q0 e H5 H7).
  apply seqdiv;trivial.
  apply seqdiv;trivial.
  rewrite <- H4 in H0;inversion H0.
  rewrite <- H6 in H0;inversion H0.
  apply parldiv; exact (IHp1 q0 e0 H4 H10).
  apply parrdiv; exact (IHp2 t e0 H7 H10).
  rewrite <- H5 in H0;inversion H0.
   apply parldiv;exact (IHp1 q0 e0 H6 H9).
   apply parrdiv;trivial.  
  rewrite <- H5 in H0;inversion H0.
   apply parldiv;trivial.
   apply parrdiv;exact (IHp2 q0 e0 H6 H9).
  rewrite <- H5 in H0;inversion H0.
  apply loopdiv.
  rewrite H3;trivial.
Qed.

(* This is the main theorem. It states that eval function returns one possible trace of the
process received. It just requires a non-diverging process as argument. *)
Theorem eval_correct: forall (p:Proc), ~(Diverge p) -> isTrace (eval p) p.
Proof.
  cofix H.
  intros p HD.
  trace_unfold (eval p);unfold trace_decompose;unfold eval;cbv iota beta;fold eval.
  case_eq p;intros.
   constructor.
   constructor.
   (* Prefix *)
   case_eq (menu (pref e p0));intros. 
     simpl choose;cbv iota beta;constructor.
    case_eq (choose (cons e0 l));intros;simpl choose.
    case_eq (smallstep_eval (pref e p0) e1);intros;cbv iota beta.
     constructor.
      inversion H3.
      case_eq (beq_event e e1);intro H4;rewrite H4 in H5;inversion H5;exact (eq_event_true e e1 H4).
      inversion H3.
      destruct (beq_event e e1);inversion H5.
      cut (~Diverge p1);intro HDp1.
      exact (H p1 HDp1).
      apply HD;rewrite H0;rewrite <- H6 in HDp1;constructor;trivial.
     constructor.
     constructor.
   (* Choice *) 
   case_eq (menu (choi p0 p1));intros.
    simpl choose;cbv iota beta;constructor.
    case_eq (choose (cons e l));intros;simpl choose.
    case_eq (smallstep_eval (choi p0 p1) e0);intros;cbv iota beta.
     cut (Op (choi p0 p1) e0 p2);intros.
     inversion H4.
      apply choitl.
      rewrite <- (eval_op p0 p2 e0 H7).
      cut (~Diverge p0);intro HDp0.
      exact (H p0 HDp0).
      apply HD;rewrite H0;apply choildiv;trivial.

      apply choitr.
      rewrite <- (eval_op p1 p2 e0 H7).
      cut (~Diverge p1);intro HDp1.
      exact (H p1 HDp1).
      apply HD;rewrite H0;apply choirdiv;trivial.

      rewrite exec_eps.
      apply (empt (eval (choi q p1)) (choi p0 p1) (choi q p1)).
       rewrite H8,H6;trivial.
       cut (~Diverge (choi q p1));intro HND.
       exact (H (choi q p1) HND).
       apply HD.
       rewrite H0.
       rewrite <- H6, <- H8 in H4;exact (div_pred (choi p0 p1) (choi q p1) Eps H4 HND).

      rewrite exec_eps.
      apply (empt (eval (choi p0 q)) (choi p0 p1) (choi p0 q)).
       rewrite H8,H6;trivial.
       cut (~Diverge (choi p0 q));intro HND.
       exact (H (choi p0 q) HND).
       apply HD;rewrite H0.
       rewrite <- H6, <- H8 in H4;exact (div_pred (choi p0 p1) (choi p0 q) Eps H4 HND).

      rewrite exec_eps.
      rewrite <- H6,H8,<-H5 in H4.
      apply (empt (eval p2) (choi skip p2) p2 H4).
       rewrite <- H8.
       cut (~Diverge p1);intro HD1.
       exact (H p1 HD1).
       apply HD;rewrite H0;apply choirdiv;trivial.

      rewrite exec_eps.
      rewrite <- H7,H8,<-H5 in H4.
      apply (empt (eval p2) (choi p2 skip) p2 H4).
       rewrite <- H8.
       cut (~Diverge p0);intro HD0.
       exact (H p0 HD0).
       apply HD;rewrite H0;apply choildiv;trivial.

      rewrite exec_eps.
      rewrite <- H6,<-H7,<-H5,<-H8 in H4.
      apply (empt (eval stop) (choi stop stop) stop H4).
       trace_unfold (eval stop);simpl;constructor.

      exact (sseval_correct e0 (choi p0 p1) p2 H3).
     constructor.
     constructor.
   (* Seq *)
   case_eq (menu (seq p0 p1));intros.
    simpl choose;cbv iota beta;constructor.
    case_eq (choose (cons e l));intros;simpl choose.
    case_eq (smallstep_eval (seq p0 p1) e0);intros.
     cut (Op (seq p0 p1) e0 p2);intros.
     inversion H4.
      apply (seqt (eval (seq q p1)) q p1 p0 e0).
       cut (~Diverge (seq q p1));intro.
       exact (H (seq q p1) H10).
       inversion H10.
       apply HD;rewrite H0.
       apply seqldiv.
       exact (div_pred p0 q e0 H9 H12).
       apply HD;rewrite H0.
       apply seqdiv;trivial.
       trivial.
  
      rewrite exec_eps.
      apply (empt (eval p2) (seq skip p2) p2).
       constructor.
       cut (~Diverge p2);intro HND.
       exact (H p2 HND).
       apply HD.
       rewrite H0;rewrite <- H8 in HND;apply seqdiv;trivial.

      rewrite exec_eps.
      apply (empt (eval stop) (seq stop p1) stop).
       constructor.
       cut (~Diverge stop);intro HND.
       exact (H stop HND).
       inversion HND.

      exact (sseval_correct e0 (seq p0 p1) p2 H3).
      constructor.
      constructor.
  (* Par *)
  case_eq (menu (par e p0 p1));intros.
   simpl choose;cbv iota beta;constructor.
   case_eq (choose (cons e0 l));intros;simpl choose.
    case_eq (smallstep_eval (par e p0 p1) e1);intros;simpl.
     cut (Op (par e p0 p1) e1 p2);intros.
     inversion H4.
      apply (parsynct (eval (par e q t)) e1 e q t p0 p1);auto.
      cut (~Diverge (par e q t));intro HND.
      exact (H (par e q t) HND).
      apply HD;rewrite H0.
      inversion HND.
      apply parldiv.
      exact (div_pred p0 q e1 H8 H14).
      apply parrdiv.
     exact (div_pred p1 t e1 H11 H14).

    apply (parlt (eval (par e q p1)) e1 e q p1 p0);auto.
    cut (~Diverge (par e q p1));intro HND.
     exact (H (par e q p1) HND).
     apply HD.
     rewrite H0.
     inversion HND.
     apply parldiv;exact (div_pred p0 q e1 H10 H13).
     apply parrdiv;trivial.
   
    apply (parrt (eval (par e p0 q)) e1 e p0 q p1);auto.
    cut (~Diverge (par e p0 q));intro HND.
    exact (H (par e p0 q) HND).
    apply HD;rewrite H0.
    inversion HND.
    apply parldiv;trivial.
    apply parrdiv;exact (div_pred p1 q e1 H10 H13).

    rewrite exec_eps.
    apply (empt (eval skip) (par e skip skip) skip).
    rewrite H5, H7, H8, H9 at 1;trivial.

    trace_unfold (eval skip);simpl;constructor.
    exact (sseval_correct e1 (par e p0 p1) p2 H3).
    constructor.
    constructor.
  (* Id *)
  simpl.
  rewrite exec_eps.
  apply (empt (eval (Env n)) (id n) (Env n)).
  constructor.
  cut (~Diverge (Env n));intro HND.
  exact (H (Env n) HND).
  apply HD;rewrite H0.
  apply loopdiv;trivial.
Qed.

(* Notes:

- S = (EOut e f) -> P || (EOut e f) -> Q 
    eval S = exec f :: eval S'
    S' = smallstep_eval e S = P || Q

  'f' function associated to the 'e' event will be executed just once. But both sides 
  will consume the event

- Each event must have associated one function at most.

*)