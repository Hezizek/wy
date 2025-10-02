Definition excluded_middle := forall p: Prop,p \/ ~p.
Definition pierce :=  forall p q : Prop,((p->q)->p)->p.

Theorem eqv: excluded_middle pierce.
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

