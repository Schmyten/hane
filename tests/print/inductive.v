Inductive P (A : Set) : forall a : A, Set := C : forall a : A, P A a
    with Q (A : Set) : Set := Q1 : Q A | Q2 : Q A.
Print P.
Print C.
