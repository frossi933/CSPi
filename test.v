Require Import Coq.Arith.EqNat.

Load evset.

Definition Menu := EvSet.

Parameter execAct : Event -> Output.


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
  abort: Op stop Eps stop
| prefok: forall (e0 e:Event)(p:Proc), Beq_event e0 e -> Op (pref e0 p) e p
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


(* Ejemplo de eval simple *)
Parameter pred:Pred.
Definition e:Event := EIn 1 pred.
Definition p : Proc := rec 0 (pref Eps (id 0)).
Eval compute in (smallstep_eval p Eps).


(*
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
*)

Lemma iselemmember: forall (e:Event)(s:EvSet), isElem e s = true <-> isMember e s.
Proof.
  intros.
  unfold iff;split;intro H.
  induction s;intros;simpl.
  inversion H.
  simpl in H.
  case_eq (beq_event e0 a);intro H0.
  apply (sing e0 a s).
  induction e0;induction a;intros;simpl; try (inversion H0).
  
  constructor.
  rewrite (beq_nat_true i i0).
  constructor.
  assumption.
  rewrite (beq_nat_true i i0).
  constructor.
  assumption.
  apply cons.
  rewrite H0 in H.
  apply IHs.
  auto.

  induction H;intros.
  inversion H.
  simpl;auto.
  simpl.
  rewrite <- (beq_nat_refl i);auto.
  simpl.
  rewrite <- (beq_nat_refl i);auto.
  simpl.
  case_eq (beq_event e0 f);intros;auto.
Qed.

Lemma not_mem:forall (e:Event)(s:EvSet), isElem e s = false -> ~(isMember e s).
Proof.
  intros e s H H1.
  rewrite <- (iselemmember e s) in H1;rewrite H1 in H;inversion H.
Qed.


Theorem sseval_correct: forall (e:Event)(p q:Proc), smallstep_eval p e = Some q -> Op p e q.
Proof.
  intros e p. (* intros mal? *)
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
  case_eq e;intro H0.
  rewrite H0 in *.
  simpl in H.
  case_eq (smallstep_eval p1 Eps);   case_eq (smallstep_eval p2 Eps) ; intros;rewrite H1 in *;rewrite H2 in *;inversion H.   
  apply (choiepsl p1 p3 p2).
  apply (IHp1 p3).
  trivial.
  apply (choiepsl p1 p0 p2).
  apply (IHp1 p0).
  trivial.
  apply (choiepsr p2 p0 p1).
  apply (IHp2 p0).
  trivial.

  intros p0 H1.
  simpl in H.
  rewrite H1 in *.
  case_eq (smallstep_eval p1 (EIn H0 p0));case_eq (smallstep_eval p2 (EIn H0 p0));intros;rewrite H2 in *;rewrite H3 in *;inversion H.   
  apply choil.
  apply IHp1.
  assumption.
  discriminate.
  apply choil.
  apply IHp1.
  assumption.
  discriminate.
  apply choir.
  apply IHp2.
  assumption.
  discriminate.

  intros a H1.
  simpl in H.
  rewrite H1 in *.
  case_eq (smallstep_eval p1 (EOut H0 a));case_eq (smallstep_eval p2 (EOut H0 a));intros;rewrite H2 in *;rewrite H3 in *;inversion H.
  apply choil.
  apply IHp1.
  assumption.
  discriminate.
  apply choil.
  apply IHp1.
  assumption.
  discriminate.
  apply choir.
  apply IHp2.
  assumption.
  discriminate.

  simpl in H.
  case_eq p1;intros;rewrite H0 in *; try (rewrite <- H0 in H;case_eq (smallstep_eval p1 e);intros;rewrite H1 in *;inversion H;constructor;apply IHp1;rewrite H0 in H1;trivial;case_eq (smallstep_eval (pref e0 p0) e);intros;rewrite H1 in *;inversion H;constructor;apply IHp1;trivial).
  case_eq e;intros;rewrite H1 in *.
  inversion H.
  rewrite <- H3.
  constructor.
  simpl in H.
  inversion H.
  simpl in H.
  inversion H.

  simpl in H.
  case_eq p1;[ intros H0 | intros H0 | intros e1 p0 H0 | intros p0 p3 H0 | intros p0 p3 H0 | intros e1 p0 p3 H0 | intros n H0 | intros n p0 H0];
  rewrite H0 in H;rewrite <- H0 in H;rewrite <- H0;case_eq p2;intros;rewrite H1 in H;rewrite <- H1 in H;rewrite <- H1;
  case_eq (isElem e e0);intro H2;rewrite H2 in H;case_eq(smallstep_eval p1 e);intros;rewrite H3 in H;
  inversion H;
  case_eq (smallstep_eval p2 e);intros;try(rewrite H4 in H);inversion H;
   try (apply sync; [exact (IHp1 p3 H3) | exact (IHp2 p4 H4) | apply (iselemmember e e0);auto ]);
   try (apply sync; [exact (IHp1 p4 H3) | exact (IHp2 p5 H4) | apply (iselemmember e e0);auto ]);
   try (apply sync; [exact (IHp1 p0 H3) | exact (IHp2 p3 H4) | apply (iselemmember e e0);auto ]);
   try (apply sync; [exact (IHp1 p5 H3) | exact (IHp2 p6 H4) | apply (iselemmember e e0);auto ]);
   try (apply sync; [exact (IHp1 p6 H3) | exact (IHp2 p7 H4) | apply (iselemmember e e0);auto ]);
   try (apply parl; [apply (IHp1 p3 H3) | exact (not_mem e e0 H2)]);
   try (apply parl; [apply (IHp1 p0 H3) | exact (not_mem e e0 H2)]);
   try (apply parl; [apply (IHp1 p4 H3) | exact (not_mem e e0 H2)]);
   try (apply parl; [apply (IHp1 p5 H3) | exact (not_mem e e0 H2)]);
   try (apply parl; [apply (IHp1 p6 H3) | exact (not_mem e e0 H2)]);
   try (apply parr; [apply (IHp2 p3 H4) | exact (not_mem e e0 H2)]);
   try (apply parr; [apply (IHp2 p0 H4) | exact (not_mem e e0 H2)]);
   try (apply parr; [apply (IHp2 p4 H4) | exact (not_mem e e0 H2)]);
   try (apply parr; [apply (IHp2 p5 H4) | exact (not_mem e e0 H2)]);
   try (apply parr; [apply (IHp2 p6 H4) | exact (not_mem e e0 H2)]).


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
  
  simpl in H.
  destruct e;simpl;inversion H.
  constructor.
 
  simpl in H.
  destruct e;simpl;inversion H.
  constructor.
Qed.
  
CoInductive Trace : Set :=
  nil : Trace+
| cons : Output -> Trace -> Trace.

Notation "e :: t" := (cons e t).

(* Successful termination or Finite Trace *)
CoInductive succTerm : Proc -> Prop :=
  st_skip : succTerm skip
| st_pref : forall (e:Event)(p:Proc), succTerm p -> succTerm (pref e p)
| st_choi : forall (p1 p2:Proc), succTerm p1 -> succTerm p2 -> succTerm (choi p1 p2)
| st_seq : forall (p1 p2:Proc), succTerm p1 -> succTerm p2 -> succTerm (seq p1 p2)
| st_par : forall (p1 p2:Proc)(s:EvSet), succTerm p1 -> succTerm p2 -> succTerm (par s p1 p2).


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
| part: forall (t1 t2 t3:Trace)(p1 p2:Proc)(s:EvSet), isTrace t1 p1 -> isTrace t2 p2 -> MergeTrace t1 t2 t3 -> isTrace t3 (par s p1 p2).



Fixpoint menu (p:Proc) : Menu := match p with
   stop => set_add Eps empty_set
 | skip => empty_set
 | pref e r => set_add e (menu r)
 | choi r q => set_union (menu r) (menu q)
 | seq skip q => set_add Eps empty_set
 | seq r q => menu r
 | par s r q => set_union (set_inter (set_inter (menu r) (menu q)) s) (set_union (set_diff (menu r) s) (set_diff (menu q) s))
 | id n => set_add Eps empty_set
 | rec n p => set_add Eps empty_set
end.

Theorem menu_correct : forall (p:Proc)(e:Event), let m=menu p in (isElem e m -> exists p':Proc, Op p e p').

Parameter tick : Output.
Parameter empty : Output.
Parameter rand : unit -> bool. (* ?? *)



Fixpoint choose (m:Menu) : option Event := match m with
  none => None
| some e none => Some e
| some e es => if rand tt then Some e
                            else choose es
end.

CoFixpoint eval (p:Proc) : Trace := match p with
  stop => nil
  skip => tick :: nil
  q => match choose (menu q) with
         None => nil
       | Some (EOut id act as e) => match smallstep_eval p e with
                                      None => execAct act :: nil
                                      Some r => execAct act :: eval r
                                    end
       | Some e => empty :: match smallstep_eval p e with
                              None => empty :: nil
                              Some r => empty :: eval r
                            end
       end
end.

Theorem eval_correct: forall (p:Proc), isTrace (eval p) p.




