Definition impl : forall P : Prop, forall Q : Prop, Prop :=
    fun P : Prop => fun Q : Prop => forall p : P, Q.

Definition and : forall P : Prop, forall Q : Prop, Prop :=
    fun P : Prop => fun Q : Prop => forall R : Prop, impl (impl P (impl Q R)) R.