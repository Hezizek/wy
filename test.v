Section a.
Variable p:Prop.
Theorem mythm: p<->p.
Proof.
split.
intro.
assumption.
intro.
assumption.
Qed.
Check mythm.
Print mythm.
End a.

Section b.
Variables p q:Prop.
Theorem thm2:p->q->p.
Proof.
intros.
assumption.
Qed.
End b.


Section c.
Variable p q r: Prop.
Theorem exercise1:(p->q)->((q->r)->(p->r)).
Proof.
intros.
apply H0.
apply H.
assumption.
Qed.
End c.

Section d.
Variable p q r: Prop.
Hypothesis H :p->q->r.
Lemma l:p/\q->r.
intros.
destruct H0.
apply H.
assumption.
assumption.
Qed.
End d.


Section e.
Theorem t1:forall p:Prop,p->p.
Proof.
intros.
assumption.
Qed.

Variables q r:Prop.
Theorem t2:(q->r)->(q->r).
Proof.
 pose proof t1.
 specialize H with (p:= (q->r)).
 assumption.
Qed.

End e.

Section f.
Variables p q r s t : Prop.
Lemma diamond: (p->q)->(p->r)->(q->r->t)->p->t.
Proof.
intros.
apply H1.
apply H.
assumption.
apply H0.
assumption.
Qed.
End f.

Section two.
Definition excluded_middle := forall p: Prop,p \/ ~p.
Definition pierce :=  forall p q : Prop,((p->q)->p)->p.

Theorem eqv: excluded_middle <-> pierce.
Proof.
unfold excluded_middle,pierce.
split.
intros.
assert (G:= H q).
specialize H with (p:=p).
destruct H.
assumption.
destruct G.
apply H0.
auto.
tauto.


intros.
specialize H with (q:=~(p\/~p)).
specialize H with (p0:=p\/~p).
apply H.
tauto.
Qed.
End two.


Section one.
Theorem Frobenius (A:Set)(p: A->Prop)(q:Prop):
 (exists x:A,q /\ p x)<->(q /\ exists x:A,p x).
split.
intros.
split.
destruct H.
destruct H.
assumption.
destruct H.
exists x.
destruct H.
assumption.
(*Half Done.*)
intros.
destruct H.
destruct H0.
exists x.
tauto.
(*also split assumption assumption*)

Axiom em:forall p:Prop,p \/ ~p.
Theorem dual_frobenius (A:Set)(p:A->Prop)(q:Prop):
 (forall x:A,q \/ p x)<->(q \/ forall x:A,p x).
Proof.
pose proof em.
firstorder.
assert (G:=H q).
destruct G.
left.
assumption.
right.
intro.
assert (G:=H0 x).
(*tauto*)
destruct G.
(*contradiction*)
elim H1.
assumption.
assumption.
Qed.