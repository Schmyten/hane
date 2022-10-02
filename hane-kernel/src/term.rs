pub enum Term<B> {
    Prop,
    Var(usize),
    App(Box<Term<B>>, Box<Term<B>>),
    Product(B, Box<Term<B>>, Box<Term<B>>),
    Abstract(B, Box<Term<B>>, Box<Term<B>>),
    Bind(B, Box<Term<B>>, Box<Term<B>>, Box<Term<B>>),
}
