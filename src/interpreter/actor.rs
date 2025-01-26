use core::panic;

use super::ast::Process;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ExtendedOption<T> {
    Zero,
    One(T),
    Two(T, T),
}

impl<T> ExtendedOption<T> {
    pub fn is_zero(&self) -> bool {
        match self {
            ExtendedOption::Zero => true,
            _ => false,
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            ExtendedOption::One(_) => true,
            _ => false,
        }
    }

    pub fn is_two(&self) -> bool {
        match self {
            ExtendedOption::Two(_, _) => true,
            _ => false,
        }
    }

    pub fn unwrap_one(self) -> T {
        match self {
            ExtendedOption::Zero => panic!("Expecting a One, found a Zero"),
            ExtendedOption::One(val) => val,
            ExtendedOption::Two(_, _) => panic!("Expecting a One, found a Two"),
        }
    }

    pub fn unwrap_two(self) -> (T, T) {
        match self {
            ExtendedOption::Zero => panic!("Expecting a Two, found a Zero"),
            ExtendedOption::One(_) => panic!("Expecting a Two, found a One"),
            ExtendedOption::Two(val1, val2) => (val1, val2),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ActorId {
    id: String,
}

impl PartialEq for ActorId {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialOrd for ActorId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self == other {
            Some(std::cmp::Ordering::Equal)
        } else if self.id.starts_with(&other.id) {
            Some(std::cmp::Ordering::Greater)
        } else if other.id.starts_with(&self.id) {
            Some(std::cmp::Ordering::Less)
        } else {
            None
        }
    }
}

impl ActorId {
    pub fn as_str<'a>(&'a self) -> &'a str {
        self.id.as_str()
    }

    pub fn root(p: &Process) -> ExtendedOption<ActorId> {
        match p {
            Process::Expr(_) => ExtendedOption::Zero,
            Process::Loop(_) => ExtendedOption::Zero,
            Process::ChanDeclaration(_, _) => ExtendedOption::One(ActorId { id: "c".to_owned() }),
            Process::Par(_, _) => ExtendedOption::Two(
                ActorId { id: "a".to_owned() },
                ActorId { id: "b".to_owned() },
            ),
            Process::Send(_, _, _) => ExtendedOption::Zero,
            Process::Receive(_, _, _) => ExtendedOption::One(ActorId { id: "c".to_owned() }),
        }
    }

    pub fn from_parent(parent: &ActorId, p: &Process) -> ExtendedOption<ActorId> {
        match p {
            Process::Expr(_) => ExtendedOption::Zero,
            Process::Loop(_) => ExtendedOption::Zero,
            Process::ChanDeclaration(_, _) => {
                ExtendedOption::One(parent.concat(&ActorId { id: "c".to_owned() }))
            }
            Process::Par(_, _) => {
                let raw_parent_clone = parent.id.clone();
                ExtendedOption::Two(
                    ActorId {
                        id: [raw_parent_clone.as_str(), "a"].concat(),
                    },
                    ActorId {
                        id: [parent.id.as_str(), "b"].concat(),
                    },
                )
            }
            Process::Send(_, _, _) => ExtendedOption::Zero,
            Process::Receive(_, _, _) => {
                ExtendedOption::One(parent.concat(&ActorId { id: "c".to_owned() }))
            }
        }
    }

    fn concat(&self, idb: &ActorId) -> ActorId {
        let raw = [self.id.clone(), idb.id.clone()].concat();
        ActorId { id: raw }
    }

    pub fn is_descendant_or_equal(&self, other: &ActorId) -> bool {
        self >= other
    }

    pub fn is_ancestor_or_equal(&self, other: &ActorId) -> bool {
        self <= other
    }

    pub fn is_descendant(&self, other: &ActorId) -> bool {
        self > other
    }

    pub fn is_ancestor(&self, other: &ActorId) -> bool {
        self < other
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::interpreter::{
        actor::ExtendedOption,
        ast::{Expression, IntExpr, Process, Value},
    };

    use super::ActorId;

    #[test]
    fn test_root() {
        assert!(ActorId::root(&Process::Expr(Expression::IntExpr(IntExpr::Lit(8)))).is_zero());
        assert!(
            ActorId::root(&Process::Expr(Expression::Val(Value::Str("".to_owned())))).is_zero()
        );

        assert_eq!(
            ActorId::root(&Process::ChanDeclaration(
                "k".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(8))))
            )),
            ExtendedOption::One(ActorId { id: "c".to_owned() }),
        );

        assert_eq!(
            ActorId::root(&Process::ChanDeclaration(
                "p".to_owned(),
                Box::new(Process::Par(
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(8)))),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(8))))
                ))
            )),
            ExtendedOption::One(ActorId { id: "c".to_owned() }),
        );

        assert_eq!(
            ActorId::root(&Process::ChanDeclaration(
                "k".to_owned(),
                Box::new(Process::ChanDeclaration(
                    "s".to_owned(),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(8))))
                ))
            )),
            ExtendedOption::One(ActorId { id: "c".to_owned() }),
        );

        assert_eq!(
            ActorId::root(&Process::Par(
                Box::new(Process::ChanDeclaration(
                    "s".to_owned(),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(8))))
                )),
                Box::new(Process::ChanDeclaration(
                    "s".to_owned(),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(8))))
                ))
            )),
            ExtendedOption::Two(
                ActorId { id: "a".to_owned() },
                ActorId { id: "b".to_owned() }
            )
        );

        assert_eq!(
            ActorId::root(&Process::Receive(
                "w".to_owned(),
                "s".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(7))))
            )),
            ActorId::root(&Process::ChanDeclaration(
                "z".to_owned(),
                Box::new(Process::Par(
                    Box::new(Process::ChanDeclaration(
                        "s".to_owned(),
                        Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9))))
                    )),
                    Box::new(Process::ChanDeclaration(
                        "s".to_owned(),
                        Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(65))))
                    ))
                ))
            ))
        );

        assert_ne!(
            ActorId::root(&Process::Receive(
                "w".to_owned(),
                "s".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(7))))
            )),
            ActorId::root(&Process::Send(
                Expression::IntExpr(IntExpr::Lit(69)),
                "y".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(7))))
            ))
        );
    }

    #[test]
    fn test_relatives() {
        let (a, b) = ActorId::root(&Process::Par(
            Box::new(Process::ChanDeclaration(
                "s".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9)))),
            )),
            Box::new(Process::ChanDeclaration(
                "s".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(65)))),
            )),
        ))
        .unwrap_two();

        assert_eq!(
            ActorId::from_parent(
                &a,
                &Process::ChanDeclaration(
                    "s".to_owned(),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9)))),
                ),
            ),
            ExtendedOption::One(ActorId {
                id: "ac".to_owned()
            })
        );

        assert_eq!(
            ActorId::from_parent(
                &b,
                &Process::ChanDeclaration(
                    "s".to_owned(),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9)))),
                ),
            ),
            ExtendedOption::One(ActorId {
                id: "bc".to_owned()
            })
        );

        assert!(ActorId::from_parent(
            &a,
            &Process::Loop(Box::new(Process::ChanDeclaration(
                "s".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9)))),
            )))
        )
        .is_zero());

        assert!(ActorId::from_parent(
            &b,
            &Process::Loop(Box::new(Process::ChanDeclaration(
                "s".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9)))),
            )))
        )
        .is_zero());

        let r = ActorId::root(&Process::Receive(
            "val".to_owned(),
            "b".to_owned(),
            Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9)))),
        ))
        .unwrap_one();
        let r1 = ActorId::from_parent(
            &r,
            &Process::Receive(
                "v".to_owned(),
                "w".to_owned(),
                Box::new(Process::Expr(Expression::Val(Value::Char(57)))),
            ),
        )
        .unwrap_one();

        assert!(ActorId::from_parent(
            &r1,
            &Process::Send(
                Expression::IntExpr(IntExpr::Lit(69)),
                "y".to_owned(),
                Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(7)))),
            ),
        )
        .is_zero());

        let (a, b) = ActorId::from_parent(
            &r1,
            &Process::Par(
                Box::new(Process::ChanDeclaration(
                    "s".to_owned(),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9)))),
                )),
                Box::new(Process::ChanDeclaration(
                    "s".to_owned(),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(65)))),
                )),
            ),
        )
        .unwrap_two();

        assert_eq!(
            ActorId::from_parent(
                &a,
                &Process::ChanDeclaration(
                    "s".to_owned(),
                    Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(9)))),
                ),
            ),
            ExtendedOption::One(ActorId {
                id: "ccac".to_owned()
            })
        );
        assert!(
            ActorId::from_parent(&b, &Process::Expr(Expression::IntExpr(IntExpr::Lit(2))))
                .is_zero()
        );
    }

    #[test]
    fn test_ord() {
        assert!(ActorId { id: "c".to_owned() } == ActorId { id: "c".to_owned() });
        assert!(
            ActorId {
                id: "ca".to_owned()
            } == ActorId {
                id: "ca".to_owned()
            }
        );
        assert!(
            ActorId {
                id: "ca".to_owned()
            } != ActorId {
                id: "cb".to_owned()
            }
        );
        assert!(
            !(ActorId {
                id: "ca".to_owned()
            } >= ActorId {
                id: "cb".to_owned()
            })
        );
        assert!(
            !(ActorId {
                id: "ca".to_owned()
            } <= ActorId {
                id: "cb".to_owned()
            })
        );
        assert!(
            ActorId {
                id: "ccac".to_owned()
            } <= ActorId {
                id: "ccacb".to_owned()
            }
        );
        assert!(
            ActorId {
                id: "ccac".to_owned()
            } < ActorId {
                id: "ccacb".to_owned()
            }
        );
        assert!(
            ActorId {
                id: "cabc".to_owned()
            } >= ActorId {
                id: "ca".to_owned()
            }
        );
        assert!(
            ActorId {
                id: "cabc".to_owned()
            } > ActorId {
                id: "ca".to_owned()
            }
        );
        assert!(
            !(ActorId {
                id: "ccaccb".to_owned()
            } >= ActorId {
                id: "ccbccb".to_owned()
            })
        );
        assert!(
            !(ActorId {
                id: "ccaccb".to_owned()
            } <= ActorId {
                id: "ccbccb".to_owned()
            })
        );
        assert!(
            !(ActorId {
                id: "aaa".to_owned()
            } <= ActorId {
                id: "aab".to_owned()
            })
        );
        assert!(
            !(ActorId {
                id: "aaa".to_owned()
            } >= ActorId {
                id: "aab".to_owned()
            })
        );
        assert!(
            ActorId {
                id: "aa".to_owned()
            } <= ActorId {
                id: "aab".to_owned()
            }
        );
        assert!(
            ActorId {
                id: "aa".to_owned()
            } < ActorId {
                id: "aab".to_owned()
            }
        );
    }
}
