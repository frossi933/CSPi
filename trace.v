Load proc.

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

