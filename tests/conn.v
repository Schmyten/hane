Definition unop : Prop := forall P : Prop, Prop.
Definition binop : Prop := forall P : Prop, forall Q : Prop, Prop.

Definition impl : binop :=
    fun P : Prop => fun Q : Prop => forall p : P, Q.

Definition and : binop :=
    fun P : Prop => fun Q : Prop => forall R : Prop, impl (impl P (impl Q R)) R.

Definition or : binop :=
    fun P : Prop => fun Q : Prop => forall R : Prop,
        impl (impl (impl P R) (impl (impl Q R) R)) R.

Definition not : unop :=
    fun P : Prop => forall Q : Prop, impl P Q.
