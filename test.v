Require Import Coq.Arith.EqNat.

Load evset.

Definition Menu := EvSet.

Definition Name : Set := nat.

(* Expresiones del lenguaje CSP *)
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
| id : Name -> Proc
| rec : Name -> Proc -> Proc.


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
 (* abort:     Op stop Eps stop*)
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
| recursion: forall (p:Proc)(n:Name), Op (rec n p) Eps (sust (rec n p) n p)
| idref:     forall (n:Name), Op (id n) Eps (id n). (* o "p" directamente *)
  
Fixpoint smallstep_eval (p:Proc) (e:Event) : option Proc :=
  match p, e with 
(*    stop, Eps => Some stop*)
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
                       | Some p3, Some p4 => Some (choi p3 p2) (* hacerlo random *)
                       end
  | choi p1 p2, v => match smallstep_eval p1 v, smallstep_eval p2 v with
                       None, None => None
                     | Some p3, None => Some p3
                     | None, Some p4 => Some p4
                     | Some p3, Some p4 => Some p3 (* hacerlo random *)
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
  | rec n p, Eps => (* chequear n *)Some (sust (rec n p) n p)
  | id n, Eps => Some (id n) (* ?? *)
  | p , v => None
  end.


(* Ejemplo de eval simple *)
Parameter pred:Pred.
Definition e:Event := EIn 1 pred.
Definition p : Proc := rec 0 (pref Eps (id 0)).
Eval compute in (smallstep_eval p Eps).


Lemma iselemmember: forall (e:Event)(s:EvSet), isElem e s = true <-> isMember e s.
Proof.
  intros.
  unfold iff;split;intro H.
  induction s;intros;simpl;inversion H.
  simpl in H.
  case_eq (beq_event e0 a);intro H0.
  apply (sing e0 a s).
  induction e0;induction a;intros;simpl;try (inversion H0).  
  constructor.
  rewrite (beq_nat_true i i0).
  constructor.
  assumption.
  rewrite (beq_nat_true i i0).
  constructor.
  assumption.
  rewrite (beq_nat_true i i0).
  constructor.
  assumption.
  rewrite (beq_nat_true i i0).
  constructor.
  assumption.
  apply consmem.
  rewrite H0 in H.
  apply IHs;auto.

  induction H;intros.
  inversion H.
  simpl;auto.
  simpl;rewrite <- (beq_nat_refl i);auto.
  simpl;rewrite <- (beq_nat_refl i);auto.
  simpl;case_eq (beq_event e0 f);intros;auto.
  simpl;rewrite <- (beq_nat_refl i);auto.
  simpl;rewrite <- (beq_nat_refl i);auto.
  simpl;rewrite <- (beq_nat_refl i);auto.
  simpl.
  destruct (beq_event e0 f);simpl;auto.
Qed.

Lemma not_mem:forall (e:Event)(s:EvSet), isElem e s = false -> ~(isMember e s).
Proof.
  intros e s H H1.
  rewrite <- (iselemmember e s) in H1;rewrite H1 in H;inversion H.
Qed.

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
   apply (prefok e0 e p0 (eq_event_true e0 e H0)).
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
  case_eq p1;[ intros H0 | intros H0 | intros e1 p0 H0 | intros p0 p3 H0 | intros p0 p3 H0 | intros e1 p0 p3 H0 | intros n H0 | intros n p0 H0];
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
 
  (* REC *)
  simpl in H.
  destruct e;simpl;inversion H.
  constructor.
Qed.
  
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

Ltac trace_unfold_hyp t := match goal with
  h: context[t] |- _ => rewrite (trace_dec_lemma t) in h
end.

(* Successful termination or Finite Trace *)
CoInductive succTerm : Proc -> Prop :=
  st_skip : succTerm skip
| st_pref : forall (e:Event)(p:Proc), succTerm p -> succTerm (pref e p)
| st_choi : forall (p1 p2:Proc), succTerm p1 -> succTerm p2 -> succTerm (choi p1 p2)
| st_seq : forall (p1 p2:Proc), succTerm p1 -> succTerm p2 -> succTerm (seq p1 p2)
| st_par : forall (p1 p2:Proc)(s:EvSet), succTerm p1 -> succTerm p2 -> succTerm (par s p1 p2).


CoFixpoint concatt (t1 t2:Trace) : Trace := match t1 with
  nilt => t2
| o :: t1' => o :: (concatt t1' t2)
end.

CoInductive MergeTrace : Trace -> Trace -> Trace -> Prop :=
  mt_nil1: forall t:Trace, MergeTrace nilt t t
| mt_cons1: forall (o:Output)(t1 t2 t3:Trace), MergeTrace t1 t2 t3 -> 
                                               MergeTrace (o :: t1) t2 (o :: t3)
| mt_nil2: forall t:Trace, MergeTrace t nilt t
| mt_cons2: forall (o:Output)(t1 t2 t3:Trace), MergeTrace t1 t2 t3 -> 
                                               MergeTrace t1 (o :: t2) (o :: t3).

Parameter execAct : Event -> Output.
Parameter test : Pred -> bool.
Parameter tick : Output.
Parameter empty : Output.
Parameter rand : unit -> bool. (* ?? *)

Axiom exec_eps : execAct Eps = empty.

CoInductive isTrace : Trace -> Proc -> Prop :=
  skip_tick : isTrace (tick :: nilt) skip    (* ?? *)
| empt:       forall (t:Trace)(p1 p2:Proc), Op p1 Eps p2 -> 
                                        isTrace t p2 -> 
                                        isTrace (empty :: t) p1 (* ?? *)
| nil_is_t :  forall p:Proc, isTrace nilt p (* ?? *)
| preft :     forall (t:Trace)(p:Proc)(e:Event), isTrace t p -> isTrace ((execAct e)::t) (pref e p)
| choitl :    forall (t:Trace)(p1 p2:Proc), isTrace t p1 -> isTrace t (choi p1 p2)
| choitr :    forall (t:Trace)(p1 p2:Proc), isTrace t p2 -> isTrace t (choi p1 p2)
(*| choitepsl :  forall (t:Trace)(p1 p2 p3:Proc), Op p1 Eps p3 -> isTrace t (choi p3 p2) -> isTrace t (choi p3 p2)
| choitepsr :  forall (t:Trace)(p1 p2 p3:Proc), Op p2 Eps p3 -> isTrace t (choi p1 p2) -> isTrace t (choi p1 p3)*)
| seqt:       forall (t1 t2:Trace)(p1 p2:Proc), isTrace t1 p1 -> succTerm p1 -> isTrace t2 p2 -> isTrace (concatt t1 t2) (seq p1 p2)
| part:       forall (t1 t2 t3:Trace)(p1 p2:Proc)(s:EvSet), isTrace t1 p1 -> isTrace t2 p2 -> MergeTrace t1 t2 t3 -> isTrace t3 (par s p1 p2).


Fixpoint menu (p:Proc) : Menu := match p with
(*   stop => set_add Eps empty_set *)
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
 | rec n p => set_add Eps empty_set
end.

Theorem menu_correct : forall (p:Proc)(e:Event), isMember e (menu p) -> exists p':Proc, Op p e p'.

Definition choose (m:Menu) : option Event := 
match m with
  nil => None
| cons e es => Some e
end.
(*Fixpoint choose (m:Menu) : option Event := match m with
  nil => None
| cons e es => match es with
                 nil => Some e
               | cons f fs => if rand tt then Some e
                                         else choose es
               end
end.
*)
CoFixpoint eval (p:Proc) : Trace := match p with
  stop => nilt
| skip => tick :: nilt
| q => match choose (menu q) with
         None => nilt (* esperar que se active alguno o morir *)
       | Some e => execAct e :: match smallstep_eval q e with
                                  None => nilt
                                | Some r => eval r
                                end
       end
end.

Theorem eval_correct: forall (p:Proc), isTrace (eval p) p.
Proof.
  cofix H.
  induction p0;intros;simpl.
  trace_unfold (eval stop);constructor.
  trace_unfold (eval skip);constructor.
  trace_unfold (eval (pref e0 p0)).
   case_eq e0;intros;simpl.
    constructor;auto.
    destruct (test p1);simpl.
     rewrite <- (beq_nat_refl i).  
     constructor;auto.
     constructor.
    rewrite <- (beq_nat_refl i).
    constructor;auto.
  trace_unfold (eval (choi p0_1 p0_2)).
   unfold eval.

   unfold trace_decompose.
   case_eq (menu (choi p0_1 p0_2));intros.
   simpl.
   constructor.
   simpl choose.
   cbv iota beta.
   case_eq (smallstep_eval (choi p0_1 p0_2) e0);intros.
   inversion H0.
 
  case_eq p0_1;case_eq p0_2;intros.
   simpl.
   rewrite exec_eps.
   apply (empt (eval stop) (choi stop stop) stop).
   apply choistop.

   rewrite <- H1;exact IHp0_1.
  
   simpl.
   rewrite exec_eps.
   apply (empt (eval stop) (choi stop skip) stop).
   apply skipr.
   rewrite <- H1;exact IHp0_1.

   simpl.   
   destruct e0;simpl. (* prohibir pref Eps p0 ? *)
   

   Focus 2.
   simpl.
   case_eq (test p1);intros;simpl.
   rewrite <- (beq_nat_refl i).
   apply choitr.
   constructor.
   rewrite H0 in IHp0_2.
   trace_unfold_hyp (eval (pref (EIn i p1) p0)).
   simpl in IHp0_2.
   rewrite H2 in IHp0_2;simpl in IHp0_2.
   rewrite <- (beq_nat_refl i) in IHp0_2.
   inversion IHp0_2.
   inversion H5.
   inversion H12.
   assumption.
   apply choitl;constructor.

   Focus 3.
   apply choitr.
   unfold eval.
   unfold trace_decompose.
   case_eq (menu (choi stop (choi p0 p1)));intros.
   simpl.
   constructor.
   simpl choose.
   cbv iota beta.
   case_eq (smallstep_eval (choi stop (choi p0 p1)) e0);intros.
   inversion H2.
   destruct p0;destruct p1;inversion H4.
   cbv delta.
   simpl.
  
   apply (preft (eval p0) p0 (EIn i p1)) in IHp0_2.
   inversion IHp0_2.
   exact (H p0).
   Guarded.
   destruct (rand tt);simpl.
   Focus 2.
   apply choitr.
   constructor.
   rewrite H0 in IHp0_2.
   trace_unfold_hyp (eval (pref (EIn i p1) p0)).
   simpl in IHp0_2.
   unfold eval in IHp0_2.
   simpl in IHp0_2.
   inversion IHp0_2.
   exact (H p0).
   Guarded.
   rewrite exec_eps.
   apply empt.
   apply (H (choi stop (pref Eps p0))).
   Guarded.
   apply (choitepsl (eval (choi stop (pref Eps p0))) stop (pref Eps p0) stop).
   constructor.