use super::ast::Process;

#[derive(Clone)]
pub struct ActorId {
    id: String,
}

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

    //TODO: implement unwrapper functions
}

impl ActorId {
    pub fn root(p: Process) -> ExtendedOption<ActorId> {
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

    pub fn from_parent(parent: &ActorId, p: Process) -> ExtendedOption<ActorId> {
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

    pub fn is_descendant_or_equal(&self, idb: &ActorId) -> bool {
        self.id.starts_with(&idb.id)
    }

    pub fn is_ancestor_or_equal(&self, idb: &ActorId) -> bool {
        idb.is_descendant_or_equal(self)
    }

    pub fn is_descendant(&self, idb: &ActorId) -> bool {
        self.id.starts_with(&idb.id) && self.id != idb.id
    }

    pub fn is_ancestor(&self, idb: &ActorId) -> bool {
        idb.is_descendant(self)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_relatives() {
        //TODO
    }
}
