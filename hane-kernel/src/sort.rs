use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, PartialEq, Eq)]
pub enum Sort {
    Prop,
    Set,
    Type(usize),
}

impl Display for Sort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Sort::Prop => write!(f, "Prop"),
            Sort::Set => write!(f, "Set"),
            Sort::Type(n) => write!(f, "Type{{{n}}}"),
        }
    }
}

impl PartialOrd for Sort {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Sort {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Sort::Prop, Sort::Prop) => Ordering::Equal,
            (Sort::Prop, Sort::Set) => Ordering::Less,
            (Sort::Prop, Sort::Type(_)) => Ordering::Less,
            (Sort::Set, Sort::Prop) => Ordering::Greater,
            (Sort::Set, Sort::Set) => Ordering::Equal,
            (Sort::Set, Sort::Type(_)) => Ordering::Less,
            (Sort::Type(_), Sort::Prop) => Ordering::Greater,
            (Sort::Type(_), Sort::Set) => Ordering::Greater,
            (Sort::Type(l), Sort::Type(r)) => l.cmp(r),
        }
    }
}

impl Sort {
    pub fn ttype(&self) -> Self {
        match self {
            Sort::Prop => Sort::Type(0),
            Sort::Set => Sort::Type(0),
            Sort::Type(n) => Sort::Type(n+1),
        }
    }

    pub fn product(self, body: Self) -> Self {
        if body == Sort::Prop {
            Sort::Prop
        } else {
            self.max(body)
        }
    }
}

